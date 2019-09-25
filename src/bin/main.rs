use std::mem::size_of;
use std::rc::Rc;

use mc2wasm::runtime::*;

pub fn main() -> Result<(), Error> {
    let runtime = &mut Runtime::new();

    let parent_class = StructClassDef::new("Parent", &["age", "name", "child"]);
    let child_class = StructClassDef::new("Child", &["age", "name", "parent"]);

    runtime.add_class(parent_class);
    runtime.add_class(child_class);

    let parent_class = runtime.class_by_name("Parent").unwrap();
    let child_class = runtime.class_by_name("Child").unwrap();

    let obj: Rc<Box<dyn Object>> = Rc::new(parent_class.instantiate(runtime, &[])?);
    let child: Rc<Box<dyn Object>> = Rc::new(child_class.instantiate(runtime, &[])?);
    child.set_field(runtime, "parent", Some(Rc::clone(&obj)))?;
    child.set_field(runtime, "age", new_value(12))?;
    obj.set_field(runtime, "child", new_value("pepito"))?;

    //let obj = runtime::Object::Integer(12);
    obj.set_field(runtime, "age", new_value("cachito"))?;
    println!("Child = {:?}", obj.get_field(runtime, "child")?);
    println!("Age = {:?}", child.get_field(runtime, "age")?);
    println!("Sizeof<Value> = {}", size_of::<Value>());

    let array = new_array(vec![new_value("juan"), new_value(123)]).unwrap();
    println!(
        "array[3] = {:?}",
        array.invoke(runtime, "[]=", &[new_value(1), new_value("cacho")])?
    );
    println!(
        "array[3] = {:?}",
        array.invoke(runtime, "[]", &[new_value(1)])?
    );
    // let neg = new_value(34).unwrap().invoke("-", &[])?;
    // array.set_field("len", None)?;
    let raw = Rc::into_raw(new_value(23).unwrap());

    println!("sizeof = {:?}", size_of::<*const dyn Object>());
    Ok(())
}
