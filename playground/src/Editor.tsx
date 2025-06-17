import {
  CompilationDiagnostics,
  compile,
  type JsPos,
  type JsToken,
  tokenize,
  print_ast,
} from "neon-web";
import { createEffect, createSignal, onCleanup, onMount, Show } from "solid-js";
import * as monaco from "monaco-editor";

const rangeFromLocation = ({
  end,
  start,
}: { start: JsPos; end: JsPos }): monaco.IRange => ({
  endColumn: end.col,
  startColumn: start.col,
  endLineNumber: end.line,
  startLineNumber: start.line,
});

export function Editor(props: {
  onContentChange: (val: string) => void;
  content: string;
}) {
  const [astView, setAstView] = createSignal(false);
  let monacoEl: HTMLDivElement | undefined;
  let editor: monaco.editor.IStandaloneCodeEditor | undefined;

  let syntaxDecorations: monaco.editor.IEditorDecorationsCollection | undefined;
  let diagnosticsDecorations:
    | monaco.editor.IEditorDecorationsCollection
    | undefined;
  let runtimeDecorations:
    | monaco.editor.IEditorDecorationsCollection
    | undefined;

  onMount(() => {
    if (!monacoEl) return;
    monaco.languages.register({ id: "neon", extensions: ["neon"] });
    monaco.languages.setLanguageConfiguration("neon", {
      brackets: [
        ["{", "}"],
        ["(", ")"],
        ["[", "]"],
      ],
      surroundingPairs: [
        { open: "{", close: "}" },
        { open: '"', close: '"' },
        { open: "[", close: "]" },
        { open: "(", close: ")" },
      ],
    });

    editor = monaco.editor.create(monacoEl, {
      language: "neon",
      theme: "vs-dark",
      fixedOverflowWidgets: true,
      glyphMargin: true,
      automaticLayout: true,
    });

    syntaxDecorations = editor.createDecorationsCollection();
    runtimeDecorations = editor.createDecorationsCollection();
    diagnosticsDecorations = editor.createDecorationsCollection();

    editor.getModel()?.onDidChangeContent(() => {
      const value = editor?.getValue();
      if (!value) return;
      syntaxDecorations?.clear();
      runtimeDecorations?.clear();
      diagnosticsDecorations?.clear();
      renderDiagnostics(value);
      renderSyntaxHighlighting(value);
      props.onContentChange(value);
    });
  });

  onCleanup(() => editor?.dispose());

  createEffect(() => editor?.setValue(props.content));
  createEffect(() => {});

  function diagnosticsToDecorations(
    e: CompilationDiagnostics,
  ): monaco.editor.IModelDeltaDecoration[] {
    return e.errors.map((e) => ({
      range: rangeFromLocation(e.loc),
      options: {
        inlineClassName: "diagnostic",
        className: "diagnostic-container",
        glyphMarginClassName: "diagnostic-glyph",
        hoverMessage: {
          value: e.message,
        },
      },
    }));
  }

  function renderDiagnostics(src: string) {
    try {
      compile(src);
    } catch (e) {
      if (!(e instanceof CompilationDiagnostics)) return;
      const decorations = diagnosticsToDecorations(e);
      diagnosticsDecorations?.append(decorations);
    }
  }

  function renderSyntaxHighlighting(src: string) {
    if (!syntaxDecorations) return;

    const tokens: JsToken[] = tokenize(src);
    const highlights: monaco.editor.IModelDeltaDecoration[] = tokens.map(
      (it) => ({
        range: rangeFromLocation(it),
        options: {
          inlineClassName: it.kind,
        },
      }),
    );
    syntaxDecorations.append(highlights);
  }

  const astString = () => {
    if (!astView()) return;
    try {
      return print_ast(props.content);
    } catch {
      return null;
    }
  };

  return (
    <div class="relative w-full h-full">
      <div
        style={{
          "grid-template-columns": astView()
            ? "minmax(0px, 1fr) 1fr"
            : "minmax(0px, 1fr)",
        }}
        class="grid h-full w-full"
      >
        <div ref={monacoEl} />
        <Show when={astView()}>
          <code class="overflow-y-auto p-5 max-h-[90vh]">
            <pre>{astString()}</pre>
          </code>
        </Show>
      </div>
      <button
        onClick={() => setAstView(!astView())}
        type="button"
        class="absolute bottom-0 right-0 text-white z-10 p-2 m-2 bg-gray-700 rounded-3xl"
      >
        AST View
      </button>
    </div>
  );
}
