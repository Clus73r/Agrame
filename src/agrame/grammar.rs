use core::fmt;
use std::{collections::HashMap};
use id_arena::{Arena, Id};
use rand::{seq::IteratorRandom, thread_rng, rngs::ThreadRng};

type Result<T> = std::result::Result<T, GrammarError>;
type ParseResult<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct GrammarError;

#[derive(Debug, Clone)]
pub struct ParseError;

type ProductionId = Id<Production>;
type TerminalId = Id<Terminal>;
type NonTerminalId = Id<NonTerminal>;

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct NonTerminal {
    name: String,
    productions: Vec<ProductionId>,
}

impl fmt::Display for NonTerminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct Terminal {
    name: String,
}

impl fmt::Display for Terminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Production {
    Terminal(TerminalId),
    NonTerminal(NonTerminalId),
    Sequence(Vec<ProductionId>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Symbol {
    Terminal(TerminalId),
    NonTerminal(NonTerminalId),
}

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct Grammar {
    terminals: Arena<Terminal>,
    non_terminals: Arena<NonTerminal>,
    productions: Arena<Production>,
    terminal_map: HashMap<String, TerminalId>,
    non_terminal_map: HashMap<String, NonTerminalId>,
    start_symbol: Option<Symbol>,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn process_production(grammar: &Grammar, production: &Production) -> String {
            match production {
                Production::Terminal(t) => format!("{}", grammar.terminals[*t]),
                Production::NonTerminal(n) => format!("{}", grammar.non_terminals[*n]),
                Production::Sequence(s) => {
                    s.iter()
                        .map(|e| process_production(grammar, &grammar.productions[*e]))
                        .reduce(|a, e| a + ", " + &e).unwrap_or("".to_string())
                },
            }
        }
        let terminals = self.terminals.iter()
            .map(|(id, t)| {
                format!("{:?}: {}", id, t)
            })
            .reduce(|a, e| a + "\n" + &e).unwrap_or("".to_string());
        let non_terminals = self.non_terminals.iter()
            .map(|(id, t)| {
                let productions = t.productions.iter()
                    .map(|e| process_production(self, &self.productions[*e]))
                    .reduce(|a, e| a + "\n" + &e).unwrap_or("".to_string());
                format!("{:?}: {}", id, t) + &format!("\n{}", productions)
            })
            .reduce(|a, e| a + "\n" + &e).unwrap_or("".to_string());
        write!(f, "Terminal:\n{}\nNonTerminals:\n{}", terminals, non_terminals)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ProductionBuilder {
    Terminal(String),
    NonTerminal(String),
    Sequence(Vec<ProductionBuilder>),
}

impl Grammar {
    pub fn new() -> Self {
        Grammar {
            terminals: Arena::new(),
            non_terminals: Arena::new(),
            productions: Arena::new(),
            terminal_map: HashMap::new(),
            non_terminal_map: HashMap::new(),
            start_symbol: None,
        }
    }

    pub fn add_terminal(&mut self, name: &str) {
        if self.terminal_map.contains_key(name) { return; }
        let t = self.terminals.alloc(Terminal{ name: name.to_string() });
        self.terminal_map.insert(name.to_string(), t);
    }

    pub fn add_non_terminal(&mut self, name: &str) {
        if self.non_terminal_map.contains_key(name) { return; }
        let nt = self.non_terminals.alloc(NonTerminal{ name: name.to_string(), productions: Vec::new() });
        self.non_terminal_map.insert(name.to_string(), nt);
        if self.start_symbol.is_none() {
            let _ = self.set_start_symbol(name);
        }
    }

    pub fn set_start_symbol(&mut self, name: &str) -> Result<()> {
        if let Some(nt) = self.non_terminal_map.get(name) {
            self.start_symbol = Some(Symbol::NonTerminal(*nt));
            return Ok(());
        }
        if let Some(t) = self.terminal_map.get(name) {
            self.start_symbol = Some(Symbol::Terminal(*t));
            return Ok(());
        }
        Err(GrammarError)
    }

    fn produce_production(&mut self, production: ProductionBuilder) -> Result<ProductionId> {
        match production {
            ProductionBuilder::Terminal(name) => {
                self.add_terminal(&name);
                match self.terminal_map.get(&name) {
                    Some(terminal) => Ok(self.productions.alloc(Production::Terminal(*terminal))),
                    None => Err(GrammarError),
                }
            },
            ProductionBuilder::NonTerminal(name) => {
                self.add_non_terminal(&name);
                match self.non_terminal_map.get(&name) {
                    Some(non_terminal) => Ok(self.productions.alloc(Production::NonTerminal(*non_terminal))),
                    None => Err(GrammarError),
                }
            },
            ProductionBuilder::Sequence(seq) => {
                let mapped: Result<Vec<ProductionId>> = seq.into_iter().map(|e| self.produce_production(e)).collect();
                match mapped {
                    Ok(seq) => Ok(self.productions.alloc(Production::Sequence(seq))),
                    Err(_) => Err(GrammarError),
                }
            },
        }
    }

    pub fn add_production(&mut self, non_terminal: &str, production: ProductionBuilder) -> Result<ProductionId> {
        let production = self.produce_production(production)?;
        match self.non_terminal_map.get(non_terminal) {
            Some(nt) => {
                self.non_terminals.get_mut(*nt).unwrap().productions.push(production);
                Ok(production)
            }
            None => Err(GrammarError),
        }
    }

    pub fn terminal_iter(&self) -> id_arena::Iter<'_, Terminal, id_arena::DefaultArenaBehavior<Terminal>> {
        self.terminals.iter()
    }

    pub fn non_terminal_iter(&self) -> id_arena::Iter<'_, NonTerminal, id_arena::DefaultArenaBehavior<NonTerminal>> {
        self.non_terminals.iter()
    }

    pub fn production_iter(&self, non_terminal: &str) -> impl Iterator + '_ {
        let nt = self.non_terminals.get(*self.non_terminal_map.get(non_terminal).unwrap()).unwrap();
        nt.productions.iter().map(|p| self.productions.get(*p).unwrap())
    }

    pub fn produce(&self) -> String {
        let mut rng = thread_rng();
        fn apply_production(grammar: &Grammar, production: &Production, rng: &mut ThreadRng) -> String {
            match production {
                Production::Terminal(t) => grammar.terminals.get(*t).unwrap().name.clone(),
                Production::NonTerminal(nt) => {
                    let next = grammar.non_terminals.get(*nt).unwrap();
                    let x = next.productions.iter().choose(rng).unwrap();
                    apply_production(grammar, grammar.productions.get(*x).unwrap(), rng)
                },
                Production::Sequence(seq) => {
                    seq.iter()
                        .map(|p| apply_production(grammar, grammar.productions.get(*p).unwrap(), rng))
                        .reduce(|a, p| a + &p).unwrap()
                    // let x = seq.iter().choose(rng).unwrap();
                },
            }
        }
        println!("{}", self);
        match self.start_symbol.as_ref().expect("no start symbol") {
            Symbol::Terminal(t) => self.terminals.get(*t).unwrap().name.clone(),
            Symbol::NonTerminal(nt) => {
                self.non_terminals.get(*nt).unwrap().productions.iter()
                    .map(|p| apply_production(self, self.productions.get(*p).unwrap(), &mut rng))
                    .reduce(|a, p| a + &p).unwrap()
            }
        }
    }

    pub fn parse_text<'a>(&self, text: &'a str) -> (&'a str, ParseResult<ParseTree>) {
        match self.start_symbol.clone().unwrap() {
            Symbol::Terminal(t) => {
                self.try_terminal(text, self.terminals.get(t).unwrap())
            }
            Symbol::NonTerminal(nt) => {
                self.non_terminals.get(nt).unwrap().productions.iter()
                    .map(|p| self.try_production(text, self.productions.get(*p).unwrap()))
                    .find(|e| e.1.is_ok()).unwrap()
            }
        }
    }

    fn try_production<'a>(&self, text: &'a str, production: &Production) -> (&'a str, ParseResult<ParseTree>) {
        todo!()
    }

    fn try_terminal<'a>(&self, text: &'a str, terminal: &Terminal) -> (&'a str, ParseResult<ParseTree>) {
        if terminal.name.len() > text.len() {
            return (text, Err(ParseError));
        }

        let text_slice = &text[..terminal.name.len()];
        if text_slice == terminal.name {
            (&text[terminal.name.len()..], Ok(ParseTree{
                node: ParseNode::Terminal(ParseTerminal{name: terminal.name.to_string()}),
                children: Vec::new()
            }))
        } else {
            (text, Err(ParseError))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseTree {
    node: ParseNode,
    children: Vec<ParseTree>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseNode {
    Terminal(ParseTerminal),
    NonTerminal(ParseNonTerminal),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseTerminal {
    name: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseNonTerminal {
    name: String,
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn grammar0() {
        let mut grammar_a = Grammar::new();
        grammar_a.add_terminal("a");
        grammar_a.add_terminal("b");
        grammar_a.add_non_terminal("A");
        grammar_a.add_non_terminal("B");
        grammar_a.add_non_terminal("C");
        let prod0 = grammar_a.add_production("A", ProductionBuilder::Terminal("a".to_string()));
        let prod1 = grammar_a.add_production("A", ProductionBuilder::Terminal("b".to_string()));
        let _ = grammar_a.set_start_symbol("A");

        let correct_terminals = &vec!["a", "b"];
        let correct_terminal_iter: Vec<&str> = correct_terminals.iter().map(AsRef::as_ref).collect();
        let terminals: Vec<&str> = grammar_a.terminal_iter().map(|(_, t)| t.name.as_str()).collect();
        let cmp = correct_terminal_iter.eq(&terminals);
        if !cmp {
            println!("Correct Terminals:");
            for terminal in correct_terminals {
                println!("{}", terminal);
            }
            println!("Found Terminals:");
            for terminal in terminals {
                println!("{}", terminal);
            }
        }
        assert!(cmp);

        let correct_non_terminals = &vec!["A", "B", "C"];
        let correct_non_terminal_iter: Vec<&str> = correct_non_terminals.iter().map(AsRef::as_ref).collect();
        let non_terminals: Vec<&str> = grammar_a.non_terminal_iter().map(|(_, t)| t.name.as_str()).collect();
        let cmp = correct_non_terminal_iter.eq(&non_terminals);
        if !cmp {
            println!("Correct non_terminals:");
            for non_terminal in correct_non_terminals {
                println!("{}", non_terminal);
            }
            println!("Found non_terminals:");
            for non_terminal in non_terminals {
                println!("{}", non_terminal);
            }
        }
        assert!(cmp);

        let correct_productions = &vec![
            Production::Terminal(*grammar_a.terminal_map.get("a").unwrap()),
            Production::Terminal(*grammar_a.terminal_map.get("b").unwrap()),
        ];
        let cmp = correct_productions.iter().eq(grammar_a.productions.iter().map(|(_, e)| e));
        if !cmp {
            println!("Correct productions:");
            for production in correct_productions {
                println!("{:?}", production);
            }
            println!("Found productions:");
            for production in grammar_a.productions.iter() {
                println!("{:?}", production);
            }
        }
        assert!(cmp);

        let correct_production_ids = &vec![prod0, prod1];
        let non_terminal = grammar_a.non_terminals.get(*grammar_a.non_terminal_map.get("A").unwrap()).unwrap();
        let map = non_terminal.productions.iter().map(|e| *e);
        let other = correct_production_ids.iter().map(|e| e.clone().unwrap());
        let cmp = map.clone().eq(other.clone());
        if !cmp {
            println!("Correct production ids:");
            for production in other {
                println!("{:?}", production);
            }
            println!("Found production ids:");
            for production in map {
                println!("{:?}", production);
            }
        }
        assert!(cmp);

    }

    #[test]
    fn parse0() {
        let grammar = crate::grammar_parse::parse("a -> \"a\"").unwrap();
        let (rem, Ok(tree)) = grammar.parse_text("a")
        else {todo!()};

        assert_eq!(tree, ParseTree{
            node: ParseNode::Terminal(ParseTerminal{
                name: "a".to_string(),
            }),
            children: Vec::new(),
        });
        assert_eq!(rem, "");
    }

    #[test]
    fn parse1() {
        let grammar = crate::grammar_parse::parse("a -> \"a\"").unwrap();
        let (rem, Ok(tree)) = grammar.try_terminal("a", &Terminal{
            name: "a".to_string(),
        })
        else {
            todo!()
        };

        assert_eq!(tree, ParseTree{
            node: ParseNode::Terminal(ParseTerminal{
                name: "a".to_string(),
            }),
            children: Vec::new(),
        });
        assert_eq!(rem, "");
    }

    #[test]
    fn parse2() {
        let grammar = crate::grammar_parse::parse("a -> \"a\"").unwrap();
        let (rem, Err(ParseError)) = grammar.try_terminal("b", &Terminal{
            name: "a".to_string(),
        })
        else {
            todo!()
        };

        assert_eq!(rem, "b");
    }

    // #[test]
    // fn grammar1() {
    //     let mut grammar_a = Grammar::new();
    //     grammar_a.add_non_terminal("a");
    //     grammar_a.add_non_terminal("a");
    //     let _ = grammar_a.add_production("a", ProductionBuilder::NonTerminal("a".to_string()));
    //     println!("{}", grammar_a);
    // }

    // #[test]
    // fn grammar2() {
    //     let grammar = grammar_parse::parse("a -> \"a\"").unwrap();
    //     println!("{}", grammar.production_iter("a").count());
    // }

//     #[test]
//     fn produce0() {
//         let grammar = grammar_parse::parse("a -> \"a\"");
//         let out = grammar.unwrap().produce();
//     }
}
