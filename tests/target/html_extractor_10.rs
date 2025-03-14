pub fn main() {
    html_extractor::html! {
        <div>
        </div>
        if html_handler.test() {
            <div>
            </div>
        } => value = value;
        <div>
        </div>
    };
}
