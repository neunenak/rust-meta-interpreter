use std::collections::HashMap;
use colored::*;
use std::fmt::Write;
use std::time;

pub struct LLVMCodeString(pub String);

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct EvalOptions {
  pub execution_method: ExecutionMethod,
  pub debug_passes: HashMap<String, PassDebugOptionsDescriptor>,
}

#[derive(Debug, Hash, PartialEq)]
pub struct PassDescriptor {
  pub name: String,
  pub debug_options: Vec<String>
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PassDebugOptionsDescriptor {
  pub opts: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ExecutionMethod {
  Compile,
  Interpret,
}
impl Default for ExecutionMethod {
  fn default() -> ExecutionMethod {
    ExecutionMethod::Interpret
  }
}

#[derive(Debug, Default)]
pub struct UnfinishedComputation {
  artifacts: Vec<(String, TraceArtifact)>,
  pub durations: Vec<time::Duration>,
  pub cur_debug_options: Vec<String>,
}

#[derive(Debug)]
pub struct FinishedComputation {
  artifacts: Vec<(String, TraceArtifact)>,
  durations: Vec<time::Duration>,
  text_output: Result<String, String>,
}

impl UnfinishedComputation {
  pub fn add_artifact(&mut self, artifact: TraceArtifact) {
    self.artifacts.push((artifact.stage_name.clone(), artifact));
  }
  pub fn finish(self, text_output: Result<String, String>) -> FinishedComputation {
    FinishedComputation {
      artifacts: self.artifacts,
      text_output,
      durations: self.durations
    }
  }
  pub fn output(self, output: Result<String, String>) -> FinishedComputation {
    FinishedComputation {
      artifacts: self.artifacts,
      text_output: output,
      durations: self.durations,
    }
  }
}

impl FinishedComputation {
  pub fn to_repl(&self) -> String {
    let mut buf = String::new();
    for (stage, artifact) in self.artifacts.iter() {
      let color = artifact.text_color;
      let stage = stage.color(color).bold();
      let output = artifact.debug_output.color(color);
      write!(&mut buf, "{}: {}\n", stage, output).unwrap();
    }

    let debug_timing = true;
    if debug_timing {
      write!(&mut buf, "Timing: ").unwrap();
      for duration in self.durations.iter() {
        let timing = (duration.as_secs() as f64) + (duration.subsec_nanos() as f64 * 1e-9);
        write!(&mut buf, "{}s, ", timing).unwrap()
      }
      write!(&mut buf, "\n").unwrap();
    }
    match self.text_output {
      Ok(ref output) => write!(&mut buf, "{}", output).unwrap(),
      Err(ref err) => write!(&mut buf, "{} {}", "Error: ".red().bold(), err).unwrap(),
    }
    buf
  }
  pub fn to_noninteractive(&self) -> Option<String> {
    match self.text_output {
      Ok(_) => {
        let mut buf = String::new();
        for (stage, artifact) in self.artifacts.iter() {
          let color = artifact.text_color;
          let stage = stage.color(color).bold();
          let output = artifact.debug_output.color(color);
          write!(&mut buf, "{}: {}\n", stage, output).unwrap();
        }
        if buf == "" { None } else { Some(buf) }
      },
      Err(ref s) => Some(format!("{} {}", "Error: ".red().bold(), s))
    }
  }
}

#[derive(Debug)]
pub struct TraceArtifact {
  stage_name: String,
  debug_output: String,
  text_color: &'static str,
}

impl TraceArtifact {
  pub fn new(stage: &str, debug: String) -> TraceArtifact {
    let color = match stage {
      "parse_trace"  | "ast" => "red",
      "ast_reducing" => "red",
      "tokens" => "green",
      "type_check" => "magenta",
      _ => "blue",
    };
    TraceArtifact { stage_name: stage.to_string(), debug_output: debug, text_color: color}
  }

  pub fn new_parse_trace(trace: Vec<String>) -> TraceArtifact {
    let mut output = String::new();

    for t in trace {
      output.push_str(&t);
      output.push_str("\n");
    }

    TraceArtifact { stage_name: "parse_trace".to_string(), debug_output: output, text_color: "red"}
  }
}

pub trait ProgrammingLanguageInterface {
  fn execute_pipeline(&mut self, _input: &str, _eval_options: &EvalOptions) -> FinishedComputation {
    FinishedComputation { artifacts: vec![], text_output: Err(format!("Execution pipeline not done")), durations: vec![] }
  }

  fn get_language_name(&self) -> String;
  fn get_source_file_suffix(&self) -> String;
  fn get_passes(&self) -> Vec<PassDescriptor> {
    vec![]
  }
  fn handle_custom_interpreter_directives(&mut self, _commands: &Vec<&str>) -> Option<String> {
    None
  }
  fn custom_interpreter_directives_help(&self) -> String {
    format!(">> No custom interpreter directives specified <<")
  }
  fn get_doc(&mut self, _commands: &Vec<&str>) -> Option<String> {
    None
  }
}

/* a pass_chain function signature looks like: 
 * fn(&mut ProgrammingLanguageInterface, A, Option<&mut DebugHandler>) -> Result<B, String>
 *
 * TODO use some kind of failure-handling library to make this better
 */

#[macro_export]
macro_rules! pass_chain {
  ($state:expr, $options:expr; $($pass:path), *) => {
    |text_input| {
      let mut comp = UnfinishedComputation::default();
      pass_chain_helper! { ($state, comp, $options); text_input $(, $pass)* }
    }
  };
}

#[macro_export]
macro_rules! pass_chain_helper {
  (($state:expr, $comp:expr, $options:expr); $input:expr, $pass:path $(, $rest:path)*) => {
    {
      use std::time;
      use schala_repl::PassDebugOptionsDescriptor;
      let pass_name = stringify!($pass);
      let (output, duration) = {
        let ref debug_map = $options.debug_passes;
        let debug_handle = match debug_map.get(pass_name) {
          Some(PassDebugOptionsDescriptor { opts }) => {
            let ptr = &mut $comp;
            ptr.cur_debug_options = opts.clone();
            Some(ptr)
          }
          _ => None
        };
        let start = time::Instant::now();
        let pass_output = $pass($state, $input, debug_handle);
        let elapsed = start.elapsed();
        (pass_output, elapsed)
      };
      $comp.durations.push(duration);
      match output {
        Ok(result) => pass_chain_helper! { ($state, $comp, $options); result $(, $rest)* },
        Err(err) => {
          $comp.output(Err(format!("Pass {} failed with {:?}", pass_name, err)))
        }
      }
    }
  };
  // Done
  (($state:expr, $comp:expr, $options:expr); $final_output:expr) => {
    {
      let final_output: FinishedComputation = $comp.finish(Ok($final_output));
      final_output
    }
  };
}
