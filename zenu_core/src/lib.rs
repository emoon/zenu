use core::cell::{UnsafeCell, RefCell};

struct Inner {
    memory: Vec<u8>,
    offset: usize,
}

struct Allocator<'a> {
    inner: RefCell<Inner>, 
    phantom: core::marker::PhantomData<&'a Inner>,
}

struct Ui<'a> {
    allocator: Allocator<'a>,
    root: Option<&'a Widget<'a>>,
}

impl <'a>Allocator<'a> {
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(Inner {
                memory: vec![0; 1024], 
                offset: 0,
            }),
            phantom: core::marker::PhantomData,
        }
    }

    #[inline(always)]
    pub fn allocate<T: Default + Sized>(&'a self) -> &'a mut T {
        let size = std::mem::size_of::<T>();
        let mut inner = self.inner.borrow_mut();
        let start = inner.offset;

        inner.offset += size;

        unsafe 
        {
            let ptr = inner.memory.as_mut_ptr().add(start) as *mut T;
            let t = &mut *ptr;
            *t = T::default();
            t 
        }
    }
}

#[derive(Debug, Default)]
pub enum SizeKind {
    #[default]
    Null,
    Pixels,
    TextContent,
    PercentOfParent,
    ChildrenSum,
}

#[derive(Debug, Default)]
pub struct Size {
    kind: SizeKind,
    value: f32,
    strictness: f32,
}

pub enum Axis2 {
    X,
    Y,
}

#[derive(Debug, Default)]
pub struct Rect {
    position: [f32; 2],
    size: [f32; 2],
}

#[derive(Debug, Default)]
pub struct WidgetInner {
    pref_size: [Size; 2],
    computed_rel_position: [f32; 2],
    fixed_size: [f32; 2],
    rect: Rect,
}

#[derive(Debug, Default)]
pub struct Widget<'a> {
    inner: UnsafeCell<WidgetInner>,
    parent: Option<&'a Widget<'a>>,
    first: Option<&'a Widget<'a>>,
    next: Option<&'a Widget<'a>>,
}

pub fn calc_sizes_standalone(root: &Widget, axis: usize) {
    let axis = axis & 1;
    let inner = unsafe { &mut *root.inner.get() }; 
    match inner.pref_size[axis].kind {
        SizeKind::Pixels => inner.fixed_size[axis] = inner.pref_size[axis].value,
        SizeKind::TextContent => {
            let padding = inner.pref_size[axis].value;
            let text_size = 10.0; // TODO: get text size
            inner.fixed_size[axis] = padding + text_size;
        },
        _ => {},
    }

    let mut node = root.first;

    while let Some(next) = node {
        calc_sizes_standalone(next, axis);
        node = next.next;
    }
}

pub fn test() {
    let ui = Ui {
        allocator: Allocator::new(),
        root: None,
    };

    let root = ui.allocator.allocate::<Widget>();
    let child = ui.allocator.allocate::<Widget>();
    let child2 = ui.allocator.allocate::<Widget>();
    let child3 = ui.allocator.allocate::<Widget>();

    child2.next = Some(child3);
    child.next = Some(child2);
    root.next = Some(child);

    calc_sizes_standalone(root, 0);

    let mut node = root.next;

    while let Some(next) = node {
        println!("next = {:?}", node);
        node = next.next;
    }

    /*
    let mut node = root.next;

    loop {
        if let Some(next) = node {
            println!("next = {:?}", node);
            node = next.next;
        } else {
            break;
        }
    }
    */

    //let allocator = Allocator::new();
    //let x = allocator.allocate::<u32>();
    //let y = allocator.allocate::<u32>();

    println!("x = {:?}", root);
}


struct Zenu {


}

struct Zenu {


}
