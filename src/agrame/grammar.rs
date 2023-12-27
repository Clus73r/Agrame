pub struct NonTerminal {
    name: String,
    production: Vec<Production>,
}

pub struct Production {
    produces: Vec<Terminal>,
}

pub struct Terminal {
    value: String,
}
