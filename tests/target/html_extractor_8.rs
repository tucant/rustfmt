// rustfmt-error_on_unformatted: true
pub fn main() {
    {
        html_extractor::html!(
            // commentmatters
            <tr>_
                <td class="tbsubhead dl-inner" >_
                    <p><strong><a href=module_url>module_id<span class="eventTitle">module_name</span></a></strong></p>_
                    <p>lecturer</p>_
                </td>_
        );
    }
}
