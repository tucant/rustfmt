pub fn main() {
    html_extractor::html! {
<p>
let location_or_additional_info = if html_handler.peek().is_some() {
let location_or_additional_info = html_handler.next_any_child();
</p>_
} => location_or_additional_info; else {
</p>_
} => ();
<div>
</div>
    };
}
