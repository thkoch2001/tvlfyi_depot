use genawaiter::rc::{Co, Gen};
use std::cell::RefCell;
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;

#[derive(Debug)]
enum ValueRepr {
    Int(i64),
    Thunk((i64, i64)),
}

#[derive(Clone, Debug)]
struct Value(Rc<RefCell<ValueRepr>>);

impl Value {
    fn force(&self) {
        let mut inner = self.0.borrow_mut();
        match *inner {
            ValueRepr::Int(_) => return,
            ValueRepr::Thunk((a, b)) => {
                *inner = ValueRepr::Int(a + b);
            }
        }
    }

    fn is_forced(&self) -> bool {
        matches!(*self.0.borrow(), ValueRepr::Int(_))
    }

    fn int(&self) -> i64 {
        match *self.0.borrow() {
            ValueRepr::Int(i) => i,
            ValueRepr::Thunk(_) => panic!("unforced thunk!"),
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value(Rc::new(RefCell::new(ValueRepr::Int(value))))
    }
}

impl From<(i64, i64)> for Value {
    fn from(value: (i64, i64)) -> Self {
        Value(Rc::new(RefCell::new(ValueRepr::Thunk(value))))
    }
}

async fn list_maker(values: Vec<Value>, co: Co<Value>) -> Vec<i64> {
    let mut output: Vec<i64> = vec![];

    for value in values {
        if !value.is_forced() {
            co.yield_(value.clone()).await;
        }

        output.push(value.int());
    }

    output
}

async fn list_reverser(values: Vec<Value>, co: Co<Value>) -> Vec<i64> {
    let mut output = list_maker(values, co).await;
    output.reverse();
    output
}

struct Frame {
    gen: Gen<Value, (), Pin<Box<dyn Future<Output = Vec<i64>>>>>,
}

fn pin_future(
    f: impl Future<Output = Vec<i64>> + 'static,
) -> Pin<Box<dyn Future<Output = Vec<i64>>>> {
    Box::pin(f)
}

fn main() {
    let mut frames: Vec<Frame> = vec![];

    let values: Vec<Value> = vec![
        42.into(),
        (12, 54).into(),
        4.into(),
        (40, 2).into(),
        2.into(),
    ];
    let second = values.clone();

    frames.push(Frame {
        gen: Gen::new(|co| pin_future(list_maker(values, co))),
    });

    frames.push(Frame {
        gen: Gen::new(|co| pin_future(list_reverser(second, co))),
    });

    for (idx, mut frame) in frames.into_iter().enumerate() {
        loop {
            match frame.gen.resume() {
                genawaiter::GeneratorState::Yielded(val) => {
                    println!("yielded {:?} in frame {}", val, idx);
                    val.force();
                }
                genawaiter::GeneratorState::Complete(list) => {
                    println!("result {}: {:?}", idx, list);
                    break;
                }
            }
        }
    }
}
