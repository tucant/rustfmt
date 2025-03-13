pub fn main() {
    html_extractor::html! {
        <input value={&frormat!("{id:015}")}></input>_
    };
}
