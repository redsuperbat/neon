use std::collections::HashMap;

use super::type_checker::Type;

#[derive(Clone, Debug)]
pub struct TypeEnvironment {
    bindings: Vec<HashMap<String, Type>>,
    current_scope: usize,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            bindings: vec![HashMap::new()],
            current_scope: 0,
        }
    }

    pub fn clone_with_bindings(&self) -> TypeEnvironment {
        TypeEnvironment {
            bindings: self.bindings.clone(),
            current_scope: 0,
        }
    }

    fn bindings_mut(&mut self) -> &mut HashMap<String, Type> {
        self.bindings
            .get_mut(self.current_scope)
            .expect("Internal neon error")
    }

    pub fn declare(&mut self, name: &str, t: Type) {
        self.bindings_mut().insert(name.to_string(), t);
    }

    pub fn enter_scope(&mut self) {
        self.current_scope += 1;
        self.bindings.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.current_scope == 0 {
            panic!("Cannot exit root scope")
        }
        self.current_scope -= 1;
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        let mut scope = self.current_scope;
        loop {
            let bindings = self.bindings.get(scope);
            let Some(bindings) = bindings else {
                return None;
            };

            if let Some(t) = bindings.get(name) {
                return Some(t);
            }
            scope -= 1;
        }
    }
}
