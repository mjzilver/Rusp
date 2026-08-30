use crate::parser::Object;

pub struct Args<'a> {
    pub args: &'a [Object],
}

impl<'a> Args<'a> {
    pub fn new(args: &'a [Object]) -> Self {
        Self { args }
    }

    pub fn exactly(&self, count: usize, name: &str) -> Result<(), String> {
        if self.args.len() != count {
            return Err(format!("Incorrect number of arguments for {name}"));
        }

        Ok(())
    }

    pub fn at_least(&self, count: usize, name: &str) -> Result<(), String> {
        if self.args.len() < count {
            return Err(format!(
                "Incorrect number of arguments for {name}: \
                 want at least {count}, got {}",
                self.args.len()
            ));
        }

        Ok(())
    }

    pub fn get(&self, index: usize) -> Result<&Object, String> {
        self.args
            .get(index)
            .ok_or_else(|| format!("Missing argument at index {index}"))
    }

    pub fn symbol(&self, index: usize, message: &str) -> Result<&str, String> {
        match self.get(index)? {
            Object::Symbol(symbol) => Ok(symbol),
            _ => Err(message.to_string()),
        }
    }

    pub fn list(&self, index: usize, message: &str) -> Result<&Vec<Object>, String> {
        match self.get(index)? {
            Object::List(list) => Ok(list),
            _ => Err(message.to_string()),
        }
    }

    pub fn integer(&self, index: usize, name: &str) -> Result<i64, String> {
        match self.get(index)? {
            Object::Integer(value) => Ok(*value),
            _ => Err(format!("{name} must be an integer")),
        }
    }

    pub fn bool(&self, index: usize, name: &str) -> Result<bool, String> {
        match self.get(index)? {
            Object::Bool(value) => Ok(*value),
            _ => Err(format!("{name} must be a bool")),
        }
    }
}
