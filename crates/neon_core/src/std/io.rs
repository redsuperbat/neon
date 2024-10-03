pub const IO: &str = "
let io = {
  stdout: fn print(message: string) {}
}

fn print(message: string) {
  io.stdout(message)
}
";
