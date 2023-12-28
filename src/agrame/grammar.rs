use core::fmt;
use std::{collections::HashMap};
use id_arena::{Arena, Id};

type Result<T> = std::result::Result<T, GrammarError>;

#[derive(Debug, Clone)]
pub struct GrammarError;

type ProductionId = Id<Production>;
type TerminalId = Id<Terminal>;
type NonTerminalId = Id<NonTerminal>;

#[derive(Debug, PartialEq, Eq)]
pub struct NonTerminal {
    name: String,
    productions: Vec<ProductionId>,
}

impl fmt::Display for NonTerminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Terminal {
    name: String,
}

impl fmt::Display for Terminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Production {
    Terminal(TerminalId),
    NonTerminal(NonTerminalId),
    Sequence(Vec<ProductionId>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    Terminal(TerminalId),
    NonTerminal(NonTerminalId),
}

#[derive(Debug, PartialEq, Eq)]
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
    }

    pub fn set_start_symbol(&mut self, name: &str) -> Result<()> {
        if let Some(nt) = self.non_terminal_map.get(name) {
            return Ok(self.start_symbol = Some(Symbol::NonTerminal(*nt)));
        }
        if let Some(t) = self.terminal_map.get(name) {
            return Ok(self.start_symbol = Some(Symbol::Terminal(*t)));
        }
        return Err(GrammarError)
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

    pub fn add_production(&mut self, non_terminal: &str, production: ProductionBuilder) -> Result<()> {
        let production = self.produce_production(production)?;
        match self.non_terminal_map.get(non_terminal) {
            Some(nt) => {
                self.non_terminals.get_mut(*nt).unwrap().productions.push(production);
                Ok(())
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

    pub fn production_iter(&self) -> id_arena::Iter<'_, Production, id_arena::DefaultArenaBehavior<Production>> {
        self.productions.iter()
    }
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
        let _ = grammar_a.add_production("A", ProductionBuilder::Terminal("a".to_string()));
        let _ = grammar_a.add_production("A", ProductionBuilder::Terminal("b".to_string()));
        let _ = grammar_a.set_start_symbol("A");

        let correct_terminals = &vec!["a", "b"];
        let correct_terminal_iter: Vec<&str> = correct_terminals.iter().map(AsRef::as_ref).collect();
        let terminals: Vec<&str> = grammar_a.terminal_iter().map(|(_, t)| t.name.as_str()).collect();
        let cmp = correct_terminal_iter.eq(&terminals);
        if !correct_terminal_iter.eq(&terminals) {
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
        if !correct_non_terminal_iter.eq(&non_terminals) {
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
    }

    #[test]
    fn grammar1() {
        let mut grammar_a = Grammar::new();
        grammar_a.add_non_terminal("a");
        grammar_a.add_non_terminal("a");
        let _ = grammar_a.add_production("a", ProductionBuilder::NonTerminal("a".to_string()));
        println!("{}", grammar_a);
    }

}
