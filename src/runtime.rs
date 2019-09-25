use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;
use std::rc::Weak;

use downcast_rs::Downcast;

type ClassId = usize;

pub struct Runtime {
    classes: Vec<StructClassDef>,
    class_ids: HashMap<&'static str, ClassId>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            classes: Vec::new(),
            class_ids: HashMap::new(),
        }
    }

    pub fn add_class(&mut self, mut class: StructClassDef) -> ClassId {
        let class_id = self.classes.len();
        class.id = class_id;
        self.class_ids.insert(class.name(), class_id);
        self.classes.push(class);
        class_id
    }

    pub fn class_by_id(&self, class_id: ClassId) -> Option<&StructClassDef> {
        self.classes.get(class_id)
    }

    pub fn class_by_name(&self, name: &str) -> Option<&StructClassDef> {
        self.class_ids
            .get(name)
            .and_then(|class_id| self.class_by_id(*class_id))
    }
}

pub trait ClassDef<T: ?Sized> {
    fn name(&self) -> &'static str;

    fn instantiate(&self, runtime: &Runtime, args: &[Value]) -> Result<Box<T>, Error> {
        Err(Error::SymbolNotFound("initialize"))
    }

    fn invoke(
        &self,
        runtime: &Runtime,
        target: &T,
        symbol: &'static str,
        args: &[Value],
    ) -> McResult {
        Err(Error::SymbolNotFound(symbol))
    }

    fn get_field(&self, runtime: &Runtime, target: &T, symbol: &'static str) -> McResult {
        Err(Error::SymbolNotFound(symbol))
    }

    fn set_field(
        &self,
        runtime: &Runtime,
        target: &T,
        symbol: &'static str,
        value: Value,
    ) -> Result<(), Error> {
        Err(Error::SymbolNotFound(symbol))
    }
}

pub trait Object: Debug + Downcast {
    fn invoke(&self, runtime: &Runtime, symbol: &'static str, args: &[Value]) -> McResult {
        Err(Error::SymbolNotFound(symbol))
    }

    fn get_field(&self, runtime: &Runtime, symbol: &'static str) -> McResult {
        Err(Error::SymbolNotFound(symbol))
    }

    fn set_field(
        &self,
        runtime: &Runtime,
        symbol: &'static str,
        value: Value,
    ) -> Result<(), Error> {
        Err(Error::SymbolNotFound(symbol))
    }
}

impl_downcast!(Object);

trait HasClassDef: Object {
    fn class_def<'cl, 'rt: 'cl>(&self, runtime: &'rt Runtime) -> &'cl dyn ClassDef<Self>;
}

impl<T: HasClassDef> Object for T {
    fn invoke(&self, runtime: &Runtime, symbol: &'static str, args: &[Value]) -> McResult {
        self.class_def(runtime).invoke(runtime, self, symbol, args)
    }

    fn get_field(&self, runtime: &Runtime, symbol: &'static str) -> McResult {
        self.class_def(runtime).get_field(runtime, self, symbol)
    }

    fn set_field(
        &self,
        runtime: &Runtime,
        symbol: &'static str,
        value: Value,
    ) -> Result<(), Error> {
        self.class_def(runtime)
            .set_field(runtime, self, symbol, value)
    }
}

#[derive(Debug, Clone)]
pub struct StructClassDef {
    id: ClassId,
    name: &'static str,
    field_names: Vec<&'static str>,
}

impl StructClassDef {
    pub fn new(name: &'static str, field_names: &[&'static str]) -> Self {
        Self {
            id: Default::default(),
            name,
            field_names: field_names.to_vec(),
        }
    }
}

impl ClassDef<Struct> for StructClassDef {
    fn name(&self) -> &'static str {
        self.name
    }

    fn instantiate(&self, runtime: &Runtime, args: &[Value]) -> Result<Box<Struct>, Error> {
        let instance = Struct {
            fields: self
                .field_names
                .iter()
                .map(|name| (*name, RefCell::new(None)))
                .collect(),
            class_id: self.id,
        };
        Ok(Box::new(instance))
    }

    fn get_field(&self, runtime: &Runtime, target: &Struct, symbol: &'static str) -> McResult {
        if let Some(field) = target.fields.get(symbol) {
            Ok(field.borrow().clone())
        } else {
            ClassDef::get_field(self, runtime, target, symbol)
        }
    }

    fn set_field(
        &self,
        runtime: &Runtime,
        target: &Struct,
        symbol: &'static str,
        value: Value,
    ) -> Result<(), Error> {
        if let Some(entry) = target.fields.get(symbol) {
            Ok(*entry.borrow_mut() = value)
        } else {
            ClassDef::set_field(self, runtime, target, symbol, value)
        }
    }
}

#[derive(Debug)]
pub struct Struct {
    class_id: ClassId,
    fields: HashMap<&'static str, RefCell<Value>>,
}

impl HasClassDef for Struct {
    fn class_def<'cl, 'rt: 'cl>(&self, runtime: &'rt Runtime) -> &'cl dyn ClassDef<Self> {
        runtime.class_by_id(self.class_id).unwrap()
    }
}

struct ArrayClass;

impl ClassDef<Array> for ArrayClass {
    fn name(&self) -> &'static str {
        "Lang.Array"
    }

    fn invoke(
        &self,
        runtime: &Runtime,
        target: &Array,
        symbol: &'static str,
        args: &[Value],
    ) -> McResult {
        let index = args[0].to_owned().ok_or(Error::InvalidValue(symbol))?;
        let index = *(index
            .downcast_ref::<Number>()
            .ok_or(Error::InvalidValue(symbol)))?;
        if index < 0 || index as usize >= target.len() {
            Err(Error::ArrayOutOfBounds(index))?
        }
        match symbol {
            "[]" => {
                let cell = &target[index as usize];
                Ok(cell.borrow().clone())
            }
            "[]=" => {
                let cell = &target[index as usize];
                *cell.borrow_mut() = args[1].clone();
                Ok(None)
            }
            _ => Err(Error::SymbolNotFound(symbol)),
        }
    }
}

const ARRAY_CLASS: ArrayClass = ArrayClass;

impl HasClassDef for Array {
    fn class_def<'cl, 'rt: 'cl>(&self, runtime: &'rt Runtime) -> &'cl dyn ClassDef<Self> {
        &ARRAY_CLASS
    }
}

type Number = i32;
type Boolean = bool;
type ByteArray = Vec<i8>;
type Array = Vec<RefCell<Value>>;
type WeakReference = Weak<dyn Object>;

pub type Value = Option<Rc<Box<dyn Object>>>;
pub type McResult = Result<Value, Error>;

impl Object for Number {}
impl Object for Boolean {}
impl Object for &'static str {}

#[derive(Debug, Clone)]
pub enum Error {
    ArrayOutOfBounds(Number),
    /*
    MC_EXCEPTION,
    MC_FILE_NOT_FOUND,
    MC_ILLEGAL_FRAME,
    MC_INITIALIZER_ERROR,
    MC_INVALID_VALUE,*/
    InvalidValue(&'static str),
    Exception(Rc<Box<dyn Object>>),
    NullReference,
    /*
    MC_OUT_OF_MEMORY,
    MC_PERMISSION_REQUIRED,
    MC_STACK_UNDERFLOW,
    MC_STACK_OVERFLOW,
    */
    IllegalFrame(Number),
    SymbolNotFound(&'static str), /*
                                   MC_SYSTEM_ERROR,
                                   MC_TOO_MANY_ARGUENTS,
                                   MC_TOO_MANY_TIMERS,
                                   MC_UNEXPECTED_TYPE,
                                   MC_UNHANDLED_EXCEPTION,
                                   MC_WATCHDOG_TRIPPED
                                  */
    UnmappedError,
    UnmappedSuccess,
}

impl Object for Error {}

pub fn new_value<T: Object + 'static>(v: T) -> Value {
    Some(Rc::new(Box::new(v)))
}

pub fn new_array(vector: Vec<Value>) -> Value {
    let array: Array = vector.into_iter().map(|e| RefCell::new(e)).collect();
    Some(Rc::new(Box::new(array)))
}

fn weak_reference_still_alive(instance: &Weak<Box<dyn Object>>) -> McResult {
    let value = new_value(instance.upgrade().is_some());
    Ok(value)
}

fn weak_reference_get(instance: &Weak<Box<dyn Object>>) -> McResult {
    Ok(instance.upgrade())
}
