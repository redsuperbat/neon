/* @refresh reload */
import { render } from "solid-js/web";

import "./index.css";
import { App } from "./App";

declare global {
  function on_print(...values: unknown[]): void;
}

window.on_print = () => {};

const root = document.createElement("div");
document.body.append(root);
render(() => <App />, root);
