use rocket;
use rocket::response::Content;
use rocket::http::ContentType;
use rocket_contrib::Json;
use schala_lang;
use language::{ProgrammingLanguageInterface, EvalOptions};
use WEBFILES;


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
fn interpreter_input(input: Json<Input>) -> Json<Output> {
  let mut schala = schala_lang::Schala::new();
  let code_output = schala.evaluate_in_repl(&input.source, &EvalOptions::default());
  Json(Output { text: code_output.to_string() })
}

pub fn web_main(languages: Vec<Box<ProgrammingLanguageInterface>>) {
  rocket::ignite().mount("/", routes![index, js_bundle, interpreter_input]).launch();
}
