use agrame::grammar::Grammar;
use gloo_console::log;
use ui::{grammar_text_input::GrammarTextInput, text_input::TextInput};
use web_sys::{HtmlTextAreaElement, wasm_bindgen::JsCast};
use yew::prelude::*;
use yewdux::prelude::*;
use gloo::utils::document;

pub mod ui;

#[derive(Default, Clone, PartialEq, Store)]
struct GrammarState {
    grammar: Grammar,
}

fn update() {
    let grammar_text_input: HtmlTextAreaElement = document().get_element_by_id("grammar_input").unwrap().dyn_into::<HtmlTextAreaElement>().unwrap();
    let text_input: HtmlTextAreaElement = document().get_element_by_id("text_input").unwrap().dyn_into::<HtmlTextAreaElement>().unwrap();

    let pos_grammar = agrame::grammar_parse::parse(&grammar_text_input.value());
    match pos_grammar {
        Ok(grammar) => {
            log!(format!("{}", grammar));
            text_input.set_value(&grammar.produce());
        },
        Err(_) => log!("Grammar could not be constructed."),
    };

    // log!(grammar_text_input.value());
    // log!(text_input.value());
}

#[function_component]
fn App() -> Html {
    let counter = use_state(|| 0);
    let onclick = {
        let counter = counter.clone();
        move |_| {
            let value = *counter + 1;
            counter.set(value);
        }
    };

    html! {
        <div>
            <button {onclick}>{"+1"}</button>
            <p>{*counter}</p>
            <GrammarTextInput />
            <TextInput />
        </div>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

