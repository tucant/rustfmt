// rustfmt-format_strings: true
pub fn main() {
    html_extractor::html! {
        <ul class="nav depth_2 linkItemContainer">
            <li class="tree depth_2 linkItem branchLinkItem " title="Archiv" id=_linkclass>
                <a class=_linkclass href={&format!(
                    "thisisareallylongandinterestingurlthatneedstobebrokenup{id:\
                     015}becauseitssolong"
                )}>
    };
}
