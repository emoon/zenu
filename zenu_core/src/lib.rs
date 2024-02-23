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

#[derive(Debug, Default, PartialEq)]
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
    min: [f32; 2],
    max: [f32; 2],
}

#[derive(Debug, Default)]
pub struct WidgetInner {
    pref_size: [Size; 2],
    fixed_position: [f32; 2],
    position_delta: [f32; 2],
    view_off: [f32; 2],
    computed_rel_position: [f32; 2],
    fixed_size: [f32; 2],
    flags: u32,
    child_layout_axis: usize,
    child_count: usize,
    rect: Rect,
}

static BOX_FLAG_FIXED_WIDTH: u32 = 1 << 19;
static BOX_FLAG_FIXED_HEIGHT: u32 = 1 << 20;
static BOX_FLAG_FLOATING_X: u32 = 1 << 21;
static BOX_FLAG_FLOATING_Y: u32 = 1 << 22;
static BOX_FLAG_ALLOW_OVERFLOW_X: u32 = 1 << 23;
static BOX_FLAG_ALLOW_OVERFLOW_Y: u32 = 1 << 24;
static BOX_FLAG_ANIMATE_X: u32 = 1 << 25;
static BOX_FLAG_ANIMATE_Y: u32 = 1 << 26;

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

pub fn calc_sizes_upwards_dependent(root: &Widget, axis: usize) {
    let axis = axis & 1;
    let root_inner = unsafe { &mut *root.inner.get() }; 
    let mut node = root.first;

    match root_inner.pref_size[axis].kind {
        SizeKind::PercentOfParent => {
            let mut fixed_parent = None; 
            node = root.parent;

            while let Some(p) = node {
                let pi = unsafe { &mut *p.inner.get() }; 

                if pi.flags & (BOX_FLAG_FIXED_WIDTH << axis) != 0 ||
                   pi.pref_size[axis].kind == SizeKind::Pixels ||
                   pi.pref_size[axis].kind == SizeKind::TextContent ||
                   pi.pref_size[axis].kind == SizeKind::PercentOfParent
                {
                    fixed_parent = Some(p);
                    break;
                }

                if let Some(fp) = fixed_parent {
                    let fpi = unsafe { &mut *fp.inner.get() }; 
                    let parent_size = fpi.fixed_size[axis];
                    let percent = fpi.pref_size[axis].value;
                    let size = parent_size * percent;
                    fpi.fixed_size[axis] = size;
                    root_inner.fixed_size[axis] = size;
                }

                node = p.parent; 
            }
        },

        _ => {},
    }

    while let Some(next) = node {
        calc_sizes_upwards_dependent(next, axis);
        node = next.next;
    }
}

pub fn ui_calc_sizes_downwards_dependent(root: &Widget, axis: usize) {
    let mut node = root.first;
    while let Some(next) = node {
        ui_calc_sizes_downwards_dependent(next, axis);
        node = next.next;
    }

    let axis = axis & 1;
    let root_inner = unsafe { &mut *root.inner.get() }; 

    match root_inner.pref_size[axis].kind {
        SizeKind::ChildrenSum => {
            let mut node = root.first;
            let mut sum = 0.0;
            while let Some(p) = node {
                let pi = unsafe { &mut *p.inner.get() }; 

                if pi.flags & (BOX_FLAG_FLOATING_X << axis) == 0 {
                    if axis == root_inner.child_layout_axis {
                        sum += pi.fixed_size[axis];
                    } else {
                        sum = sum.max(pi.fixed_size[axis]);
                    } 
                }

                node = p.next; 
            }

            root_inner.fixed_size[axis] = sum;
        },

        _ => {},
    }
}

pub fn enforce_constrains(root: &Widget, axis: usize) {
    let axis = axis & 1;
    let root_inner = unsafe { &mut *root.inner.get() };
    let allowed_size = root_inner.fixed_size[axis];
  
    // The "layout axis" is the direction in which children of some node are intended to be laid out.
    // fixup children sizes (if we're solving along the *non-layout* axis)
    if axis != root_inner.child_layout_axis && (root_inner.flags & (BOX_FLAG_ALLOW_OVERFLOW_X << axis) != 0) {
        let mut node = root.first;
        while let Some(p) = node {
            let pi = unsafe { &mut *p.inner.get() }; 

            if pi.flags & (BOX_FLAG_FLOATING_X << axis) == 0 {
                let child_size = pi.fixed_size[axis];
                let violation = child_size - allowed_size;
                let max_fixup = child_size;
                let fixup = violation.clamp(0.0, max_fixup);
                if fixup > 0.0 {
                    root_inner.fixed_size[axis] -= fixup;
                }
            }

            node = p.next; 
        }
    }

    if axis == root_inner.child_layout_axis && (root_inner.flags & (BOX_FLAG_ALLOW_OVERFLOW_X << axis) != 0) {
        // figure out total allowed size & total size
        let mut node = root.first;
        let mut total_allowed_size = root_inner.fixed_size[axis];
        let mut total_size = 0.0;
        let mut total_weighted_size = 0.0;
        while let Some(p) = node {
            let pi = unsafe { &mut *p.inner.get() }; 

            if pi.flags & (BOX_FLAG_FLOATING_X << axis) == 0 {
                total_size += pi.fixed_size[axis];
                total_weighted_size += pi.fixed_size[axis] * (1.0 - pi.pref_size[axis].strictness);
            }

            node = p.next; 
        }

        // if we have a violation, we need to subtract some amount from all children
        let violation = total_size - total_allowed_size;
        if violation > 0.0 {
            let mut child_fix_ups = vec![0.0f32; root_inner.child_count]; // TODO: use a fixed size array
            let mut child_fix_sum = 0.0;
            let mut node = root.first;
            let mut child_idx = 0;
        
            while let Some(p) = node {
                let pi = unsafe { &mut *p.inner.get() }; 

                if pi.flags & (BOX_FLAG_FLOATING_X << axis) == 0 {
                    let mut fixup_size_this_child = pi.fixed_size[axis] * (1.0 - pi.pref_size[axis].strictness);
                    fixup_size_this_child = fixup_size_this_child.clamp(0.0, fixup_size_this_child);
                    child_fix_ups[child_idx] = fixup_size_this_child;
                    child_fix_sum += fixup_size_this_child;
                }

                child_idx += 1;
                node = p.next;
            }

            // fixup child sizes
            let mut child_idx = 0;
            let mut node = root.first;

            while let Some(p) = node {
                let pi = unsafe { &mut *p.inner.get() }; 

                if pi.flags & (BOX_FLAG_FLOATING_X << axis) == 0 {
                    let fixup_pct = violation / total_weighted_size;
                    let fixup_pct = fixup_pct.clamp(0.0, 1.0);
                    pi.fixed_size[axis] -= child_fix_ups[child_idx] * fixup_pct;
                }

                child_idx += 1;
                node = p.next;
            }
        }
    }

    // rjf: fixup upwards-relative sizes
    if root_inner.flags & (BOX_FLAG_ALLOW_OVERFLOW_X << axis) != 0 {
        let mut node = root.first;
        while let Some(p) = node {
            let pi = unsafe { &mut *p.inner.get() }; 

            if pi.pref_size[axis].kind == SizeKind::PercentOfParent {
                pi.fixed_size[axis] = root_inner.fixed_size[axis] * pi.pref_size[axis].value;
            }

            node = p.next;
        }
    }

    // recurse
    let mut node = root.first;
    while let Some(p) = node {
        enforce_constrains(p, axis);
        node = p.next;
    }
}

pub fn layout_positions(root: &Widget, axis: usize) {
    let mut node = root.first;
    let mut layout_position = 0.0;
    let axis = axis & 1;
    let root_inner = unsafe { &mut *root.inner.get() };
    let mut bounds = 0.0;

    while let Some(p) = node {
        let pi = unsafe { &mut *p.inner.get() }; 

        let original_position = pi.rect.min[axis].min(pi.rect.max[axis]);

        if pi.flags & (BOX_FLAG_FLOATING_X << axis) == 0 {
            pi.fixed_position[axis] = layout_position;
            if axis == root_inner.child_layout_axis {
                layout_position += pi.fixed_size[axis];
                bounds += pi.fixed_size[axis];
            } else {
                bounds = bounds.max(pi.fixed_size[axis]);
            }
        }

        /*
        if pi.flags & (BOX_FLAG_ANIMATE_X << axis) != 0 {
            if pi.first_touched_build_index == pi.last_touched_build_index {
                pi.fixed_position_animated = pi.fixed_position;
            }

            //child->rect.p0.v[axis] = root->rect.p0.v[axis] + child->fixed_position_animated.v[axis] - !(child->flags&(UI_BoxFlag_SkipViewOffX<<axis))*root->view_off.v[axis];

            let t = !(pi.flags & (BOX_FLAG_SKIP_VIEW_OFF_X << axis));

            pi.rect.min = root_inner.rect.min[axis] + pi.fixed_position_animated - t * root_inner.view_off[axis];
        } 
        */

        let t = if !(pi.flags & (BOX_FLAG_ALLOW_OVERFLOW_X << axis) != 0) { 1.0 } else { 0.0 };

        pi.rect.min[axis] = root_inner.rect.min[axis] + pi.fixed_position[axis] - t * root_inner.view_off[axis];

        pi.rect.max[axis] = pi.rect.min[axis] + pi.fixed_size[axis];
        pi.rect.min[0] = pi.rect.min[0].floor();
        pi.rect.min[1] = pi.rect.min[1].floor();
        pi.rect.max[0] = pi.rect.max[0].floor();
        pi.rect.max[1] = pi.rect.max[1].floor();

        // grab new position
        let new_position = pi.rect.min[axis].min(pi.rect.max[axis]);
    
        // store position delta
        pi.position_delta[axis] = new_position - original_position;

        node = p.next;
    }

    // recurse
    let mut node = root.first;
    while let Some(p) = node {
        layout_positions(p, axis);
        node = p.next;
    }
}

pub fn layout_root(root: &Widget, axis: usize) {
    calc_sizes_standalone(root, axis);
    calc_sizes_upwards_dependent(root, axis);
    ui_calc_sizes_downwards_dependent(root, axis);
    enforce_constrains(root, axis);
    layout_positions(root, axis);
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

