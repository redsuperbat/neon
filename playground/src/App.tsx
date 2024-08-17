import init, { interpret_src } from "neon-web";
import {
  createResource,
  createSignal,
  Match,
  onMount,
  Switch,
  onCleanup,
} from "solid-js";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import init_script from "./assets/init.neon?raw";

function LoadingPage() {
  return <div aria-busy="true"></div>;
}
function ErrorPage({ error }: { error: string }) {
  return <article>{error}</article>;
}

function ExecutionPage() {
  const [output, setOutput] = createSignal("");
  let monacoEl: HTMLDivElement | undefined;
  let editor: monaco.editor.IStandaloneCodeEditor | undefined;

  onMount(() => {
    if (!monacoEl) return;
    editor = monaco.editor.create(monacoEl, {
      value: init_script,
    });
  });

  onCleanup(() => {
    editor?.dispose();
  });

  function exec() {
    const src = editor?.getValue();
    if (!src) return;
    const res = interpret_src(src);
    setOutput(res);
  }

  return (
    <div>
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
      <button onclick={() => exec()}>Execute</button>

      <p>{output()}</p>
    </div>
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
