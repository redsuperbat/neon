use std::{any::Any, collections::HashMap, ptr};

use super::type_checker::Type;

#[derive(Clone)]
pub struct TypeEnvironment {
    node_types: HashMap<usize, Type>,
    bindings: HashMap<String, Type>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            node_types: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn declare(&mut self, name: &str, t: Type, exp: Option<&dyn Any>) {
        if let Some(exp) = exp.as_ref() {
            let address = ptr::addr_of!(exp) as usize;
            self.node_types.insert(address, t.clone());
        };
        self.bindings.insert(name.to_string(), t);
    }

    pub fn get_by_name(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name)
    }

    pub fn get_by_node(&self, exp: &dyn Any) -> Option<&Type> {
        let address = ptr::addr_of!(exp) as usize;
        self.node_types.get(&address)
    }
}
