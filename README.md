# 🌙 Lume

Lume is a lightweight yet powerful programming language designed for simplicity and efficiency. It's syntax is intuitive, making it easy for developers to pick up and start building applications quickly. 

## Examples

Here's a quick glimpse of Lume's capabilities:

A simple hello-world application:
```lm
import std.io (print)

fn main() {
  print("Hello, World!");
}
```

Types can also be defined with `struct`:
```lm
import std.io (print)

struct Counter {
  value: Int32;
}

impl Counter {
  pub fn new() {
    Counter { value: 0 }
  }

  pub fn increment(self) {
    self.value++;
  }

  pub fn decrement(self) {
    self.value--;
  }

  pub fn value(self) -> Int32 {
    return self.value;
  }
}

fn main() {
  let counter = Counter::new();
  counter.increment();

  print("Value: {}", counter.value());

  counter.decrement();
  print("Value: {}", counter.value());
}
```

# License

The repository defines the reference compiler for Lume and is licensed under the [MIT License](./LICENSE.md).