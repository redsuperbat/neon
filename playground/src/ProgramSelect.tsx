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
import { createSignal } from "solid-js";

type Program = { name: string; value: string };
const examples: Program[] = [
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
      optionValue="value"
      optionTextValue="name"
      value={value()}
      itemComponent={(props) => (
        <Select.Item item={props.item}>{props.item.rawValue.name}</Select.Item>
      )}
      onChange={(e) => {
        if (e) {
          setValue(e);
          onSelect(e);
        }
      }}
      class="px-2"
    >
      <Select.Trigger>
        <Select.Value<Program>>
          {(state) => `Program: ${state.selectedOption().name}`}
        </Select.Value>
      </Select.Trigger>
      <Select.Portal>
        <Select.Content>
          <Select.Listbox />
        </Select.Content>
      </Select.Portal>
    </Select>
  );
}
