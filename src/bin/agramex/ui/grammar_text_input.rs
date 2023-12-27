use yew::prelude::*;
use web_sys::{InputEvent, HtmlTextAreaElement};

#[function_component]
pub fn GrammarTextInput() -> Html {
    let oninput = Callback::from(move |event: InputEvent| {
        let text_area_element = event.target_dyn_into::<HtmlTextAreaElement>();
        // let text_input = event.target_dyn_into::<yew::web
        agrame::grammar_parse::parse(&text_area_element.expect("Grammar text input").value());
    });
    html! {
        <textarea {oninput} id="text_display"></textarea>
    }
}


