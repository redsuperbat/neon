import initScript from "./assets/init.neon?raw";
import init, { interpret_src } from "neon-web";
import { Match, Show, Switch, createResource, createSignal } from "solid-js";
import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";

self.MonacoEnvironment = {
  getWorker: () => new EditorWorker(),
};

import { ProgramSelect } from "./ProgramSelect";
import { Editor } from "./Editor";

function LoadingPage() {
  return <div aria-busy="true" />;
}

function ErrorPage({ error }: { error: string }) {
  return <div>{error}</div>;
}

function ExecutionPage() {
  const [content, setContent] = createSignal(initScript);
  const [output, setOutput] = createSignal<string>();

  function executeProgram() {
    const src = content();
    setOutput(interpret_src(src).result);
  }

  return (
    <main
      class="grid h-screen"
      style={{
        "grid-template-rows": "1fr auto",
      }}
    >
      <Editor onContentChange={(s) => setContent(s)} content={content()} />

      <div
        class="grid p-2 items-center"
        style={{
          "grid-template-columns": "auto 1fr auto",
        }}
      >
        <ProgramSelect onSelect={(e) => setContent(e.value)} />
        <Show when={output()} fallback={<div />}>
          <div class="flex gap-3">
            <span>Program evaluated to:</span>
            <span>{output()}</span>
          </div>
        </Show>
        <button
          type="button"
          onclick={() => executeProgram()}
          class="inline-flex items-center gap-2 bg-white border border-neutral-300 rounded-md px-3 py-1.5 text-sm shadow-sm focus:outline-none focus:ring-1 focus:ring-blue-400 focus:border-blue-400 transition-all"
        >
          Execute program
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
