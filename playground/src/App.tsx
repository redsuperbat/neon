import init, {
  interpret_src,
  JsToken,
  tokenize,
  compile,
  ProgramErr,
} from "neon-web";
import {
  createResource,
  createSignal,
  Match,
  onMount,
  Switch,
  onCleanup,
  For,
} from "solid-js";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import init_script from "./assets/init.neon?raw";
import math from "./assets/examples/math.neon?raw";
import recursion from "./assets/examples/recursion.neon?raw";
import higherOrderFunctions from "./assets/examples/higher-order-functions.neon?raw";
import fizzbuzz from "./assets/examples/fizz-buzz.neon?raw";
import typeErrors from "./assets/examples/type-errors.neon?raw";

function LoadingPage() {
  return <div aria-busy="true"></div>;
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
    label: "Recursion",
    value: recursion,
  },
  {
    label: "Higher order functions",
    value: higherOrderFunctions,
  },
  {
    label: "Fizz Buzz",
    value: fizzbuzz,
  },
  {
    label: "Type errors",
    value: typeErrors,
  },
];

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
  let monacoEl: HTMLDivElement | undefined;
  let editor: monaco.editor.IStandaloneCodeEditor | undefined;
  let decorations: monaco.editor.IEditorDecorationsCollection | undefined;
  let runtimeDecorations:
    | monaco.editor.IEditorDecorationsCollection
    | undefined;

  onMount(() => {
    if (!monacoEl) return;
    editor = monaco.editor.create(monacoEl, {
      value: init_script,
      theme: "vs-dark",
    });
    decorations = editor.createDecorationsCollection();
    runtimeDecorations = editor.createDecorationsCollection();

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
    decorations?.clear();
    runtimeDecorations?.clear();
    renderSyntaxHighlighting(src);
    renderDiagnostics(src);
  }

  function setEditorContent(src: string) {
    editor?.setValue(src);
  }

  function renderDiagnostics(src: string) {
    try {
      compile(src);
    } catch (e) {
      if (!(e instanceof ProgramErr)) return;
      const model = editor?.getModel();
      if (!model) return;
      const lineNumber = e.start[0];

      const col = model.getLineMaxColumn(lineNumber);

      const decoration: monaco.editor.IModelDeltaDecoration = {
        range: {
          startLineNumber: e.start[0],
          endLineNumber: e.start[0],
          startColumn: col,
          endColumn: e.end[1],
        },
        options: {
          isWholeLine: true,
          inlineClassName: "diagnostic-container",
          after: {
            inlineClassName: "diagnostic",
            content: e.message,
          },
        },
      };
      decorations?.append([decoration]);
    }
  }

  function renderSyntaxHighlighting(src: string) {
    if (!decorations) return;

    const tokens: JsToken[] = tokenize(src);
    const highlights: monaco.editor.IModelDeltaDecoration[] = tokens.map(
      (it) => ({
        range: {
          startColumn: it.start[1],
          endColumn: it.end[1],
          startLineNumber: it.start[0],
          endLineNumber: it.end[0],
        },
        options: {
          inlineClassName: it.kind,
        },
      }),
    );
    decorations.append(highlights);
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
      if (!(e instanceof ProgramErr)) return;
      runtimeDecorations?.append([
        {
          range: {
            startColumn: e.start[1],
            endColumn: e.end[1],
            startLineNumber: e.start[0],
            endLineNumber: e.end[0],
          },
          options: {
            isWholeLine: true,
            inlineClassName: "diagnostic-container",
            after: {
              inlineClassName: "diagnostic",
              content: e.message,
            },
          },
        },
      ]);

      setOutput({
        type: "error",
        message: e.message,
      });
    }
  }

  return (
    <main class="container">
      <h1>Neon Playground</h1>
      <div
        style={{
          height: "50vh",
          "border-radius": "5px",
          overflow: "hidden",
          "margin-bottom": "10px",
        }}
        ref={monacoEl}
      ></div>

      <div role="group">
        <select
          onchange={(e) => setEditorContent(e.target.value)}
          name="select"
          aria-label="Select"
          required
        >
          <option selected disabled value="">
            Examples
          </option>
          <For each={examples}>
            {({ value, label }) => <option value={value}>{label}</option>}
          </For>
        </select>

        <button onclick={() => exec()}>Execute</button>
      </div>
      <div
        style={{
          display: "grid",
          "place-items": "center",
          "grid-template-columns": "auto 1fr",
          padding: "10px",
        }}
      >
        <span>Result:</span>
        <span
          style={{
            color: output()?.type === "error" ? "red" : "green",
          }}
        >
          {output()?.message}
        </span>
      </div>
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
