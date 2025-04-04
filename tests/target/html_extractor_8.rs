// TODO FIXME ignored rustfmt-error_on_unformatted: true
pub fn main() {
    {
        html_extractor::html!(
            // commentmatters
            <tr>
                <td class="tbsubhead dl-inner" >
                    <p><strong><a href=module_url>module_id<span class="eventTitle">module_name</span></a></strong></p>
                    <p>lecturer</p>
                </td>
        );
    }
}
