//! This module contains the `Command` type, which represents the differenet
//! things the executable can do depending on arguments.

use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Command {
    Run(RunMode),
    Repl,
    Help(Option<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RunMode {
    File(PathBuf),
    String(String),
}

const COMMANDS: [&str; 3] = ["run", "repl", "help"];

#[derive(Error, Debug, Clone)]
pub enum CommandParseError {
    #[error("'{0}' is not a command! the valid commands are {c}",
            c=COMMANDS.join(", "))]
    NotACommand(String),
    #[error("the {0} command should does not take any flags or arguments")]
    ExpectedNoArgs(String),
    #[error(
        "wrong arguments to run command. expectd `run <filename>` or `run \
             --str \"<code\"`"
    )]
    InvalidRunArgs,
    #[error("the help command can only take 0 or 1 arguments")]
    HelpCanNotTakeMoreThanOneArg,
}

// Using an internal type to parse but exposing just a single module function.
struct Parser<'a> {
    // CLI args, but with parsed arguments skipped
    args: Args<'a>,
}

type Args<'a> = &'a [&'a str];

impl Command {
    pub fn name(&self) -> &'static str {
        match self {
            Command::Run(_) => "run",
            Command::Repl => "repl",
            Command::Help(_) => "help",
        }
    }
}

type Res<T> = Result<T, CommandParseError>;
type Error = CommandParseError;

pub fn parse(args: impl Iterator<Item = String>) -> Res<Command> {
    let strings = args.collect::<Vec<String>>();
    let strs = strings.iter().map(|x| x.as_str()).collect::<Vec<&str>>();
    Parser::new(&strs).parse()
}

impl<'a> Parser<'a> {
    fn new(args: Args<'a>) -> Self {
        Self { args }
    }

    fn advance(&mut self) {
        assert_ne!(self.args.len(), 0);
        self.args = &self.args[1..];
    }

    fn peek(&self) -> Option<&'a str> {
        self.args.first().cloned()
    }

    fn pop(&mut self) -> Option<&'a str> {
        let ret = self.peek();
        if ret.is_some() {
            self.advance();
        }
        ret
    }

    fn parse(&mut self) -> Res<Command> {
        use Command::*;
        self.pop(); // Skip the executable name
        match self.pop() {
            None => Ok(Help(None)),
            Some("help") => self.parse_help().map(Command::Help),
            Some("repl") => self.expect_no_args(Repl),
            Some("run") => self.parse_run().map(Command::Run),
            Some(bad) => Err(Error::NotACommand(bad.into())),
        }
    }

    fn parse_help(&mut self) -> Res<Option<String>> {
        if self.args.len() > 1 {
            Err(Error::HelpCanNotTakeMoreThanOneArg)
        } else {
            Ok(self.pop().map(|x| x.to_string()))
        }
    }

    fn parse_run(&mut self) -> Res<RunMode> {
        if self.peek().is_some_and(|s| s.starts_with('-')) {
            if let ["--str", s] = self.args {
                Ok(RunMode::String(s.to_string()))
            } else {
                Err(Error::InvalidRunArgs)
            }
        } else if let [s] = self.args {
            Ok(RunMode::File(s.into()))
        } else {
            Err(Error::InvalidRunArgs)
        }
    }

    fn expect_no_args(&mut self, command: Command) -> Res<Command> {
        if self.args.is_empty() {
            Ok(command)
        } else {
            Err(CommandParseError::ExpectedNoArgs(
                command.name().to_string(),
            ))
        }
    }
}
