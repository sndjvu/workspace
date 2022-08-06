use std::process::ExitCode;

fn run() -> Result<(), Box<dyn std::error::Error>> {
    todo!()
}

fn main() -> ExitCode {
    if let Err(e) = run() {
        eprintln!("error: {e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
