use rocket;

use rocket::response::NamedFile;

#[get("/")]
fn index() -> Result<NamedFile, ()> {
  NamedFile::open("static/index.html").map_err(|_| ())
}

pub fn web_main() {
  rocket::ignite().mount("/", routes![index]).launch();
}
