pub fn main() {
    // ahh the if could be parsed as a struct expression so we need Restriction::NO_STRUCT_LITERAL
    html_extractor::html!(
        <div></div>
        if html_handler {
            <div></div>
        } => value = value;
        <div></div>
    );
}
