import * as monaco from "monaco-editor";
import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";

self.MonacoEnvironment = {
  getWorker: () => new EditorWorker(),
};

import init, {
  CompilationDiagnostics,
  compile,
  interpret_src,
  type JsPos,
  type JsToken,
  tokenize,
  print_ast,
} from "neon-web";
import {
  Match,
  Show,
  Switch,
  createResource,
  createSignal,
  onCleanup,
  onMount,
} from "solid-js";
import initScript from "./assets/init.neon?raw";
import { ProgramSelect } from "./ProgramSelect";

function LoadingPage() {
  return <div aria-busy="true" />;
}

function ErrorPage({ error }: { error: string }) {
  return <article>{error}</article>;
}

const rangeFromLocation = ({
  end,
  start,
}: { start: JsPos; end: JsPos }): monaco.IRange => ({
  endColumn: end.col,
  startColumn: start.col,
  endLineNumber: end.line,
  startLineNumber: start.line,
});

type Output =
  | {
      type: "error";
      message: string;
    }
  | {
      type: "ok";
      message: string;
    };

function ExecutionPage() {
  const [output, setOutput] = createSignal<Output>();
  const [disabled, setDisabled] = createSignal(false);
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

    monaco.languages.registerHoverProvider("neon", {
      provideHover() {
        return undefined;
      },
    });

    editor = monaco.editor.create(monacoEl, {
      language: "neon",
      theme: "vs-dark",
      value: initScript,
      fixedOverflowWidgets: true,
      glyphMargin: true,
      automaticLayout: true,
    });

    syntaxDecorations = editor.createDecorationsCollection();
    runtimeDecorations = editor.createDecorationsCollection();
    diagnosticsDecorations = editor.createDecorationsCollection();

    diagnosticsDecorations.onDidChange(() =>
      setDisabled((diagnosticsDecorations?.length ?? 0) > 0),
    );

    onContentChange(editor.getValue());

    editor.getModel()?.onDidChangeContent(() => {
      const value = editor?.getValue();
      if (!value) return;
      onContentChange(value);
    });
  });

  onCleanup(() => {
    editor?.dispose();
  });

  function onContentChange(src: string) {
    syntaxDecorations?.clear();
    runtimeDecorations?.clear();
    diagnosticsDecorations?.clear();
    renderDiagnostics(src);
    renderSyntaxHighlighting(src);
  }

  function setEditorContent(src: string) {
    editor?.setValue(src);
  }

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

  function exec() {
    runtimeDecorations?.clear();
    const src = editor?.getValue();
    if (!src) return;
    try {
      const res = interpret_src(src);
      setOutput({
        type: "ok",
        message: res.result,
      });
    } catch (e) {
      if (!(e instanceof CompilationDiagnostics)) return;
      setOutput({
        type: "error",
        message: e.errors
          .map((e) => `${e.loc.start.line}:${e.loc.start.col} ${e.message}`)
          .join("\n"),
      });
    }
  }

  return (
    <main
      class="grid h-screen"
      style={{
        "grid-template-rows": "1fr auto",
      }}
    >
      <div class="relative" ref={monacoEl}>
        <button
          type="button"
          class="absolute bottom-0 right-0 text-white z-10 p-3"
        >
          AST View
        </button>
      </div>

      <div
        class="grid p-2"
        style={{
          "grid-template-columns": "auto 1fr auto",
        }}
      >
        <ProgramSelect onSelect={(e) => setEditorContent(e.value)} />
        <Show when={output()?.message} fallback={<div />}>
          <div class="flex gap-3">
            <span>Script evaluated to:</span>
            <span
              style={{
                color: output()?.type === "error" ? "red" : "green",
              }}
            >
              {output()?.message}
            </span>
          </div>
        </Show>
        <button type="button" disabled={disabled()} onclick={() => exec()}>
          Execute
        </button>
      </div>
    </main>
  );
}

export function App() {
  const [resource] = createResource(() => init({}));

  return (
    <Switch fallback={<ExecutionPage />}>
      <Match when={resource.loading}>
        <LoadingPage />
      </Match>
      <Match when={resource.error}>
        <ErrorPage error={JSON.stringify(resource.error)} />
      </Match>
    </Switch>
  );
}
