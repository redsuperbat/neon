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
} from "neon-web";
import {
  For,
  Match,
  Show,
  Switch,
  createResource,
  createSignal,
  onCleanup,
  onMount,
} from "solid-js";
import array from "./assets/examples/arrays.neon?raw";
import fib from "./assets/examples/fib.neon?raw";
import fizzbuzz from "./assets/examples/fizz-buzz.neon?raw";
import helloWorld from "./assets/examples/hello-world.neon?raw";
import higherOrderFunctions from "./assets/examples/higher-order-functions.neon?raw";
import iife from "./assets/examples/iife.neon?raw";
import math from "./assets/examples/math.neon?raw";
import recursion from "./assets/examples/recursion.neon?raw";
import typeErrors from "./assets/examples/type-errors.neon?raw";
import initScript from "./assets/init.neon?raw";

function LoadingPage() {
  return <div aria-busy="true" />;
}

function ErrorPage({ error }: { error: string }) {
  return <article>{error}</article>;
}

const examples: { label: string; value: string }[] = [
  {
    label: "Math",
    value: math,
  },
  {
    label: "Array",
    value: array,
  },
  {
    label: "Recursion",
    value: recursion,
  },
  {
    label: "Higher order functions",
    value: higherOrderFunctions,
  },
  {
    label: "Fibbonachi",
    value: fib,
  },
  {
    label: "Fizz Buzz",
    value: fizzbuzz,
  },
  {
    label: "Type errors",
    value: typeErrors,
  },
  {
    label: "Hello world 👋",
    value: helloWorld,
  },
  {
    label: "iife (Immediately invoked function expression)",
    value: iife,
  },
];

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
  const [logs, setLogs] = createSignal<string[]>();
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

    // This will be called by rust code
    window.on_print = (...args) =>
      setLogs((prev) => [...(prev ?? []), ...args.map((it) => String(it))]);
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
    setLogs(undefined);
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
      const decorations = diagnosticsToDecorations(e);
      runtimeDecorations?.append(decorations);
    }
  }

  return (
    <main class="container">
      <h1>Neon Playground</h1>
      <div
        style={{
          height: "50vh",
          "border-radius": "5px",
          "margin-bottom": "10px",
        }}
        ref={monacoEl}
      />

      <div
        style={{
          display: "grid",
          "grid-template-columns": "1fr auto",
          gap: "10px",
        }}
      >
        <div>
          <select
            onchange={(e) => setEditorContent(e.target.value)}
            style={{ height: "60px" }}
            aria-invalid={disabled()}
            name="select"
            aria-label="Select"
            aria-describedby="invalid-diagnostics"
            required
          >
            <option selected disabled value="">
              Examples
            </option>
            <For each={examples}>
              {({ value, label }) => <option value={value}>{label}</option>}
            </For>
          </select>
          <Show when={disabled()}>
            <small id="invalid-diagnostics">
              Cannot execute when there are diagnostics errors
            </small>
          </Show>
        </div>
        <button
          type="button"
          style={{ height: "60px" }}
          disabled={disabled()}
          onclick={() => exec()}
        >
          Execute
        </button>
      </div>

      <div
        style={{
          "margin-bottom": "5px",
        }}
      >
        <span
          style={{
            "margin-right": "5px",
          }}
        >
          Script evaluated to:
        </span>
        <span
          style={{
            color: output()?.type === "error" ? "red" : "green",
          }}
        >
          {output()?.message}
        </span>
      </div>

      <h3>Logs</h3>
      <pre>
        <code
          style={{
            display: "flex",
            "flex-direction": "column",
          }}
        >
          <For each={logs()}>{(item) => <div>{item}</div>}</For>
        </code>
      </pre>
    </main>
  );
}

export function App() {
  const [resource] = createResource(() => init({}));

  return (
    <div style={{ padding: "20px" }}>
      <Switch fallback={<ExecutionPage />}>
        <Match when={resource.loading}>
          <LoadingPage />
        </Match>
        <Match when={resource.error}>
          <ErrorPage error={JSON.stringify(resource.error)} />
        </Match>
      </Switch>
    </div>
  );
}
