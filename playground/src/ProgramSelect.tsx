import { Select } from "@kobalte/core/select";
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
import { createSignal } from "solid-js";

type Program = { name: string; value: string };
const examples: Program[] = [
  {
    name: "Getting started",
    value: initScript,
  },
  {
    name: "Math",
    value: math,
  },
  {
    name: "Array",
    value: array,
  },
  {
    name: "Recursion",
    value: recursion,
  },
  {
    name: "Higher order functions",
    value: higherOrderFunctions,
  },
  {
    name: "Fibbonachi",
    value: fib,
  },
  {
    name: "Fizz Buzz",
    value: fizzbuzz,
  },
  {
    name: "Type errors",
    value: typeErrors,
  },
  {
    name: "Hello world 👋",
    value: helloWorld,
  },
  {
    name: "iife (Immediately invoked function expression)",
    value: iife,
  },
];

export function ProgramSelect({
  onSelect,
}: { onSelect: (prog: Program) => void }) {
  const [value, setValue] = createSignal<Program>(examples[0]);
  return (
    <Select
      options={examples}
      optionValue="name"
      optionTextValue="name"
      value={value()}
      itemComponent={(props) => (
        <Select.Item
          class="px-3 py-1.5 text-sm hover:bg-neutral-100 data-[highlighted]:bg-blue-500 data-[highlighted]:text-white cursor-pointer rounded"
          item={props.item}
          data-key={props.item.rawValue.name}
        >
          {props.item.rawValue.name}
        </Select.Item>
      )}
      onChange={(e) => {
        if (!e) return;
        setValue(e);
        onSelect(e);
      }}
      class="px-2"
    >
      <Select.Trigger class="inline-flex items-center gap-2 bg-white border border-neutral-300 rounded-md px-3 py-1.5 text-sm shadow-sm focus:outline-none focus:ring-1 focus:ring-blue-400 focus:border-blue-400 transition-all">
        <Select.Value<Program>>
          {(state) => (
            <span class="bg-white">Program: {state.selectedOption().name}</span>
          )}
        </Select.Value>
      </Select.Trigger>
      <Select.Portal>
        <Select.Content class="bg-white border border-neutral-200 rounded-md shadow-md text-sm z-50">
          <Select.Listbox class="max-h-48 overflow-y-auto" />
        </Select.Content>
      </Select.Portal>
    </Select>
  );
}
