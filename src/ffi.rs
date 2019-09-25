use std::rc::Rc;

use crate::runtime::ClassDef;
use crate::runtime::McResult;
use crate::runtime::Object;
use crate::runtime::Struct;
use crate::runtime::Value;
use crate::runtime::{Error, Runtime as McRuntime};

type McSymbolId = usize;

#[repr(C)]
pub struct McClass {
    name: McSymbol,
    methods: *const McMethodDef,
    methods_len: usize,
}

impl ClassDef<Struct> for McClass {
    fn name(&self) -> &'static str {
        self.name.to_string()
    }

    fn invoke(
        &self,
        runtime: &McRuntime,
        target: &Struct,
        symbol: &'static str,
        args: &[Value],
    ) -> McResult {
        Err(Error::SymbolNotFound(symbol))
    }

    fn get_field(&self, runtime: &McRuntime, target: &Struct, symbol: &'static str) -> McResult {
        Err(Error::SymbolNotFound(symbol))
    }

    fn set_field(
        &self,
        runtime: &McRuntime,
        target: &Struct,
        symbol: &'static str,
        value: Value,
    ) -> Result<(), Error> {
        Err(Error::SymbolNotFound(symbol))
    }
}

#[repr(C)]
pub struct McStruct {
    __unused: [u8; 0],
}

type McStatus = u32;

pub type McValue = *const Box<dyn Object>;

#[repr(C)]
pub struct McSymbol {
    id: usize,
    name: *const u8,
    name_len: usize,
}

impl McSymbol {
    fn to_string(&self) -> &'static str {
        unsafe {
            let slice = std::slice::from_raw_parts(self.name, self.name_len);
            std::str::from_utf8_unchecked(slice)
        }
    }
}

#[no_mangle]
pub static mut __MC_ERROR: bool = false;

trait ExternMethod<Args> {
    unsafe fn invoke(
        &self,
        runtime: &McRuntime,
        target: &Struct,
        args: Args,
    ) -> Option<*const Box<dyn Object>>;
}

impl ExternMethod<()>
    for unsafe extern "C" fn(&McRuntime, &Struct) -> Option<*const Box<dyn Object>>
{
    unsafe fn invoke(
        &self,
        runtime: &McRuntime,
        target: &Struct,
        args: (),
    ) -> Option<*const Box<dyn Object>> {
        self(runtime, target)
    }
}

impl ExternMethod<Option<Rc<Box<dyn Object>>>>
    for unsafe extern "C" fn(
        &McRuntime,
        &Struct,
        Option<*const Box<dyn Object>>,
    ) -> Option<*const Box<dyn Object>>
{
    unsafe fn invoke(
        &self,
        runtime: &McRuntime,
        target: &Struct,
        args: Option<Rc<Box<dyn Object>>>,
    ) -> Option<*const Box<dyn Object>> {
        self(runtime, target, args.map(Rc::into_raw))
    }
}

type McMethod = unsafe extern "C" fn(
    runtime: &McRuntime,
    target: &Struct,
    args: *const Option<*const Box<dyn Object>>,
    args_len: usize,
) -> Option<*const Box<dyn Object>>;

fn wrap_extern_method<Args>(
    method: impl ExternMethod<Args>,
) -> impl Fn(&McRuntime, &Struct, Args) -> McResult {
    move |runtime, target, args| {
        let result = unsafe { method.invoke(runtime, target, args) };
        let result = result.map(|obj| unsafe { Rc::from_raw(obj) });
        if unsafe { __MC_ERROR } {
            if let Some(value) = &result {
                if let Some(error) = value.downcast_ref::<Error>() {
                    Err(Error::UnmappedError)
                } else {
                    Ok(result)
                }
            } else {
                Ok(result)
            }
        } else {
            if let Some(value) = &result {
                if let Some(error) = value.downcast_ref::<Error>() {
                    Err(error.clone())
                } else {
                    Err(Error::UnmappedSuccess)
                }
            } else {
                Err(Error::UnmappedSuccess)
            }
        }
    }
}

#[repr(C)]
pub struct McMethodDef {
    name: McSymbol,
    method: McMethod,
}

extern "C" {
    fn __mc_get_class_defs() -> Option<&'static McClass>;
}

#[no_mangle]
pub extern "C" fn __mc_integer(i: i32) -> *const Box<dyn Object> {
    Rc::into_raw(Rc::new(Box::new(i)))
}

#[no_mangle]
pub extern "C" fn __mc_invoke(
    runtime: &McRuntime,
    target: Option<*const Box<dyn Object>>,
    symbol: &McSymbol,
    args: *const Option<*const Box<dyn Object>>,
    args_len: usize,
) -> McStatus {
    let target = target.map(|rc| unsafe { Rc::from_raw(rc) });
    //   let result = runtime.invoke(target, symbol.to_string(), ());
    12
}
