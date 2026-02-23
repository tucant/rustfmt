// rustfmt-format_strings: true
pub fn main() {
    html_extractor::html!(
        <a href={&format!("/thisisareallylongandinterestingurlthatneedstobebrokenup{id:015}becauseitissolongandextremelylong")}>"Archiv"</a>
    );
}
