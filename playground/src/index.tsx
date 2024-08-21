/* @refresh reload */
import { render } from "solid-js/web";

import "@picocss/pico/css/pico.min.css";
import "./App.css";
import { App } from "./App";

declare global {
  function on_print(...values: any[]): void;
}

window.on_print = () => {};

const root = document.getElementById("root");

render(() => <App />, root!);
