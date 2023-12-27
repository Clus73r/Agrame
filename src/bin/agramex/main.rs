use yew::prelude::*;

pub mod ui;

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
            <crate::ui::grammar_text_input::GrammarTextInput />
        </div>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

