use neon_core::compiler::Compiler;
use rustyline::{error::ReadlineError, DefaultEditor};

pub fn start() {
    let mut rl = DefaultEditor::new().expect("Could not get cmd editor");
    let mut compiler = Compiler::new();
    compiler.register_libraries();

    let mut ctrl_c = false;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                match compiler.run(&line) {
                    Ok(v) => println!("{}", v),
                    Err(e) => println!("{}", e),
                };
                ctrl_c = false;
            }
            Err(ReadlineError::Interrupted) => {
                if ctrl_c {
                    break;
                } else {
                    println!("(To exit, press Ctrl+C again or Ctrl+D)");
                    ctrl_c = true;
                }
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
