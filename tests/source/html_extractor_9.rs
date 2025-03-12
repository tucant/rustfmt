pub fn main() {
    html_extractor::html!(
        <div></div>
        if html_handler {
            <div></div>
        } => value = value;
        <div></div>
    );
}
