pub fn main() {
    html_extractor::html! {
        <input value={&format!("{id:015}")}></input>
    };
}
