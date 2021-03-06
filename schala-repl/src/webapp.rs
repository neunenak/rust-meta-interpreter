use rocket;
use rocket::State;
use rocket::response::Content;
use rocket::http::ContentType;
use rocket_contrib::Json;
use language::{ProgrammingLanguageInterface, EvalOptions};
use WEBFILES;
use ::PLIGenerator;

#[get("/")]
fn index() -> Content<String> {
  let path = "static/index.html";
  let html_contents = String::from_utf8(WEBFILES.get(path).unwrap().into_owned()).unwrap();
  Content(ContentType::HTML, html_contents)
}

#[get("/bundle.js")]
fn js_bundle() -> Content<String> {
  let path = "static/bundle.js";
  let js_contents = String::from_utf8(WEBFILES.get(path).unwrap().into_owned()).unwrap();
  Content(ContentType::JavaScript, js_contents)
}

#[derive(Debug, Serialize, Deserialize)]
struct Input {
  source: String,
}

#[derive(Serialize, Deserialize)]
struct Output {
  text: String,
}

#[post("/input", format = "application/json", data = "<input>")]
fn interpreter_input(input: Json<Input>, generators: State<Vec<PLIGenerator>>) -> Json<Output> {
  let schala_gen = generators.get(0).unwrap();
  let mut schala: Box<ProgrammingLanguageInterface> = schala_gen();
  let code_output = schala.execute_pipeline(&input.source, &EvalOptions::default());
  Json(Output { text: code_output.to_repl() })
}

pub fn web_main(language_generators: Vec<PLIGenerator>) {
  rocket::ignite().manage(language_generators).mount("/", routes![index, js_bundle, interpreter_input]).launch();
}
