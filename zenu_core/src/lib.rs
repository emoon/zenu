
use core::cell::{UnsafeCell, RefCell};
use std::marker::PhantomData;
use smallvec::SmallVec;

//static BOX_FLAG_FIXED_WIDTH: u32 = 1 << 19;
//static BOX_FLAG_FIXED_HEIGHT: u32 = 1 << 20;
static BOX_FLAG_FLOATING_X: u32 = 1 << 21;
//static BOX_FLAG_FLOATING_Y: u32 = 1 << 22;
static BOX_FLAG_ALLOW_OVERFLOW_X: u32 = 1 << 23;
//static BOX_FLAG_ALLOW_OVERFLOW_Y: u32 = 1 << 24;
//static BOX_FLAG_ANIMATE_X: u32 = 1 << 25;
//static BOX_FLAG_ANIMATE_Y: u32 = 1 << 26;

#[derive(Debug, Default)]
pub struct Rect {
    min: [f32; 2],
    max: [f32; 2],
}

#[derive(Debug, Default)]
pub struct BoxAreaInner {
    pref_size: [Size; 2],
    calc_size: [f32; 2],
    text_data: Option<TextData>,
    child_layout_axis: Axis,
    calc_rel_position: [f32; 2],
    flags: u32,
    rect: Rect,
    view_off: [f32; 2],
    #[cfg(debug_assertions)]
    display_string: String,
}

#[derive(Debug, Default)]
pub struct BoxArea {
    inner: UnsafeCell<BoxAreaInner>,
    // TODO: We want to use referecnes here
    parent: Option<usize>,
    owner: Option<usize>,
    next: Option<usize>,
    last: Option<usize>,
}

impl BoxAreaInner {
    #[inline]
    pub(crate) fn has_flag(&self, flag: u32) -> bool {
        (self.flags & flag) == flag
    }

    #[inline]
    pub(crate) fn is_floating_on(&self, axis: usize) -> bool {
        self.has_flag(BOX_FLAG_FLOATING_X << axis)
    }

    #[inline]
    pub(crate) fn is_overflowing_on(&self, axis: u32) -> bool {
        self.has_flag(BOX_FLAG_ALLOW_OVERFLOW_X << axis)
    }
}

impl BoxArea {
    #[inline]
    fn inner_borrow(&self) -> &BoxAreaInner {
        unsafe {
            &*self.inner.get()
        }
    }

    #[inline]
    fn inner_borrow_mut(&self) -> &mut BoxAreaInner {
        unsafe {
            &mut *self.inner.get()
        }
    }

    #[inline]
    fn owner<'a>(&'a self, boxes: &'a [BoxArea]) -> Option<&'a BoxArea> {
        self.owner.map(|o| &boxes[o])
    }

    #[inline]
    fn owner_mut<'a>(&'a self, boxes: &'a mut [BoxArea]) -> Option<&'a mut BoxArea> {
        self.owner.map(move |o| &mut boxes[o])
    }

    #[inline]
    fn parent<'a>(&'a self, boxes: &'a [BoxArea]) -> Option<&'a BoxArea> {
        self.parent.map(|p| &boxes[p])
    }

    #[inline]
    fn parent_mut<'a>(&'a self, boxes: &'a mut [BoxArea]) -> Option<&'a mut BoxArea> {
        self.parent.map(move |p| &mut boxes[p])
    }

    #[inline]
    fn next<'a>(&'a self, boxes: &'a [BoxArea]) -> Option<&'a BoxArea> {
        self.next.map(|n| &boxes[n])
    }

    #[inline]
    fn next_mut<'a>(&'a self, boxes: &'a mut [BoxArea]) -> Option<&'a mut BoxArea> {
        self.next.map(move |n| &mut boxes[n])
    }

    #[inline]
    fn last<'a>(&'a self, boxes: &'a [BoxArea]) -> Option<&'a BoxArea> {
        self.last.map(|l| &boxes[l])
    }

    #[inline]
    fn last_mut<'a>(&'a self, boxes: &'a mut [BoxArea]) -> Option<&'a mut BoxArea> {
        self.last.map(move |l| &mut boxes[l])
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq)]
enum SizeKind {
    #[default]
    Null,
    Pixels,
    Em,
    TextContent,
    PercentOfAncestor,
    ChildrenSum,
}

#[derive(Debug, Clone, Copy, Default)]
struct Size {
    kind: SizeKind,
    value: f32,
    strictness: f32,
}
/*
 public static Size InPixels(float value, float strictness = 1f) =>
     new(Pixels, value, strictness);
 public static Size FromTextDim(float strictness = 1f) =>
     new(TextContent, Strictness: strictness);
 public static Size FromChildren(float strictness = 1f) =>
     new(ChildrenSum, Strictness: strictness);
 public static Size InPercentOfAncestor(float value, float strictness = 1f) =>
     new(PercentOfAncestor, value, strictness);
      */

impl Size {
    pub fn in_pixels(value: f32) -> Self {
        Self {
            kind: SizeKind::Pixels,
            value,
            strictness: 1.0,
        }
    }

    pub fn in_pixels_strict(value: f32, strictness: f32) -> Self {
        Self {
            kind: SizeKind::Pixels,
            value,
            strictness,
        }
    }

    pub fn from_children() -> Self {
        Self {
            kind: SizeKind::ChildrenSum,
            value: 0.0,
            strictness: 1.0,
        }
    }

    pub fn from_children_strict(strictness: f32) -> Self {
        Self {
            kind: SizeKind::ChildrenSum,
            value: 0.0,
            strictness,
        }
    }

    pub fn in_percent_of_ancestor(value: f32) -> Self {
        Self {
            kind: SizeKind::PercentOfAncestor,
            value,
            strictness: 1.0,
        }
    }

    pub fn in_percent_of_ancestor_strict(value: f32, strictness: f32) -> Self {
        Self {
            kind: SizeKind::PercentOfAncestor,
            value,
            strictness,
        }
    }

}

#[derive(Debug)]
struct TextData {
    display_text: String,
    text_edge_padding: f32,
    paint: Paint,
}

#[derive(Debug)]
struct Paint {
    font_metrics: FontMetrics,
}

#[derive(Debug)]
struct FontMetrics {
    descent: f32,
    top: f32,
}

#[derive(Debug, Clone, Copy, Default)]
enum Axis {
    #[default]
    Horizontal,
    Vertical,
}

pub fn do_layout_axis(root: &BoxArea, boxes: &[BoxArea], axis: usize) {
    do_layout_for(root, boxes, 0);
    do_layout_for(root, boxes, 1);
}

pub fn do_layout_for(root: &BoxArea, boxes: &[BoxArea], axis: usize) {
    solve_independent_sizes_for(root, boxes, axis);
    solve_downward_dependent_sizes_for(root, boxes, axis);
    solve_upward_dependent_sizes_for(root, boxes, axis);
    solve_downward_dependent_sizes_for(root, boxes, axis);
    solve_size_violations(root, boxes, axis);
}

fn solve_independent_sizes_for(root: &BoxArea, boxes: &[BoxArea], axis: usize) {
    let inner = root.inner_borrow_mut(); 
    let size = inner.pref_size[axis];

    match size.kind {
        SizeKind::Pixels => {
            inner.calc_size[axis] = size.value;
        }
        SizeKind::Em => {
            inner.calc_size[axis] = (size.value * top_font_size()).floor();
        }
        SizeKind::TextContent => {
            if let Some(text_data) = &inner.text_data {
                let paint = &text_data.paint;

                if axis == 0 {
                    // TODO: fix me
                    let text_width = 64.0;//paint.measure_text(&text_data.display_text);
                    let text_padding = text_data.text_edge_padding;
                    inner.calc_size[axis] = (text_width + 2.0 * text_padding).floor();
                } else {
                    let metrics = &paint.font_metrics;
                    let line_height = metrics.descent - metrics.top;
                    inner.calc_size[axis] = line_height.floor();
                }
            } else {
                panic!("box.text_data should not be None");
            }
        }
        _ => {}
    }

    // recurse
    let mut node = root.owner(boxes);
    while let Some(p) = node {
        solve_independent_sizes_for(p, boxes, axis);
        node = p.next(boxes);
    }

}

fn solve_upward_dependent_sizes_for(root: &BoxArea, boxes: &[BoxArea], axis: usize) {
    let inner = root.inner_borrow_mut(); 
    let size = inner.pref_size[axis];

    if size.kind != SizeKind::PercentOfAncestor {
        return;
    }

    let mut ancestor: Option<&BoxArea> = None;
    let mut parent = root.parent(boxes);

    while let Some(p) = parent {
        let p_borrowed = p.inner_borrow();
        if p_borrowed.pref_size[axis].kind != SizeKind::ChildrenSum {
            ancestor = Some(p);
            break;
        }
        parent = p.parent(boxes);
    }

    if let Some(a) = ancestor {
        inner.calc_size[axis] = a.inner_borrow().calc_size[axis] * size.value;
    } else {
        println!("{} is left out of size calculations!", inner.display_string);
    }
}

fn solve_downward_dependent_sizes_for(root: &BoxArea, boxes: &[BoxArea], axis: usize) {
    let mut node = root.owner(boxes);
    while let Some(next) = node {
        solve_downward_dependent_sizes_for(next, boxes, axis);
        node = next.next(boxes);
    }

    let axis = axis & 1;
    let inner = root.inner_borrow_mut(); 
    let size = inner.pref_size[axis];

    if size.kind != SizeKind::ChildrenSum {
        return;
    }

    let mut node = root.owner(boxes);
    let mut sum = 0.0;
    while let Some(p) = node {
        let pi = p.inner_borrow(); 

        if axis == inner.child_layout_axis as usize {
            sum += pi.calc_size[axis];
        } else {
            sum = f32::max(sum, pi.calc_size[axis]);
        } 

        node = p.next(boxes); 
    }

    inner.calc_size[axis] = sum;

}

fn solve_size_violations(root: &BoxArea, boxes: &[BoxArea], axis: usize) {
    let inner = root.inner_borrow_mut();
    let available_space = inner.calc_size[axis];

    let mut taken_space = 0.0;
    let mut total_fixup_budget = 0.0;
    let mut children: SmallVec<[&BoxArea; 256]> = SmallVec::new();
    let mut non_floating_children: SmallVec<[&BoxArea; 128]> = SmallVec::new();
        
    let mut node = root.owner(boxes);

    while let Some(p) = node {
        let pi = p.inner_borrow(); 

        if !pi.is_floating_on(axis) {
            non_floating_children.push(p);
        }

        children.push(p);

        node = p.next(boxes); 
    }

    if !inner.is_overflowing_on(axis as u32) {
        let mut node = root.owner;

        for p in &non_floating_children {
            let pi = p.inner_borrow(); 

            let child_axis_size = pi.calc_size[axis];
            if pi.child_layout_axis as usize == axis {
                taken_space += child_axis_size;
            } else {
                taken_space = f32::max(taken_space, child_axis_size);
            }

            let fixup_budget_this_child = child_axis_size * (1.0 - pi.pref_size[axis].strictness);
            total_fixup_budget += fixup_budget_this_child;
        }
    }

    if !inner.is_overflowing_on(axis as u32) {
        let violation = taken_space - available_space;
        if violation > 0.0 && total_fixup_budget > 0.0 {
            for p in &non_floating_children {
                let pi = p.inner_borrow_mut(); 

                let fixup_budget_this_child = pi.calc_size[axis] * (1.0 - pi.pref_size[axis].strictness);

                let fixup_size_this_child = if inner.child_layout_axis as usize == axis {
                    fixup_budget_this_child * (violation / total_fixup_budget)
                } else {
                    pi.calc_size[axis] - available_space
                }
                .clamp(0.0, fixup_budget_this_child);

                pi.calc_size[axis] -= fixup_size_this_child;
            }
        }
    }

    if inner.child_layout_axis as usize == axis {
        let mut cur_pos = 0.0;
        for child in &non_floating_children {
            let ci = child.inner_borrow_mut();
            ci.calc_rel_position[axis] = cur_pos;
            cur_pos += ci.calc_size[axis];
        }
    } else {
        for child in &non_floating_children {
            // TODO: Validate
            let ci = child.inner_borrow_mut();
            ci.calc_rel_position[axis] = 0.0;

            /* 
            if ci.fill_implicit_layout_axis {
                ci.calc_size[axis] = inner.calc_size[axis];
            }
            */
        }
    }

    for child in &children {
        let ci = child.inner_borrow_mut();
        let parent_pos = if ci.is_floating_on(axis) {
            0.0
        } else {
            inner.rect.min[axis]
        };

        ci.rect.min[axis] = parent_pos + ci.calc_rel_position[axis] - inner.view_off[axis];
        ci.rect.max[axis] = ci.rect.min[axis] + ci.calc_size[axis];
    }

    for child in &children {
        solve_size_violations(child, boxes, axis);
    }
}

fn top_font_size() -> f32 {
    // Dummy function to represent the top font size
    16.0
}

impl Paint {
    fn measure_text(&self, text: &str) -> f32 {
        // Dummy function to represent text measurement
        text.len() as f32 * 8.0
    }
}

struct Layout {
    // TODO: Arena
    owner: Vec<usize>,
    pref_width: Vec<Size>,
    pref_height: Vec<Size>,
    fixed_x: Vec<f32>,
    fixed_y: Vec<f32>,
    flags: Vec<u32>,
    child_layout_axis: Vec<Axis>,
    root: usize,
    current_parent: Option<usize>,
    boxes: Vec<BoxArea>,
}

impl Layout {
    pub fn new() -> Self {
        Self {
            pref_width: Vec::new(),
            pref_height: Vec::new(),
            fixed_x: Vec::new(),
            fixed_y: Vec::new(),
            flags: Vec::new(),
            child_layout_axis: Vec::new(),
            root: 0,
            current_parent: None,
            boxes: Vec::new(),
            owner: Vec::new(),
        }
    }

    pub fn create_box(&mut self) {
        let parent_index = self.owner.last().copied().unwrap_or_default();
        let box_area = self.create_box_inner(parent_index); 
        let parent = &mut self.boxes[parent_index];

        if let Some(p) = parent.last {
            let p = &mut self.boxes[p];
            p.last = Some(box_area);
        } else {
            // if the parent has no children, set the first child to the new box
            parent.next = Some(box_area);
            parent.last = Some(box_area);
        }
    }

    pub fn create_box_inner(&mut self, parent_index: usize) -> usize {
        let index = self.boxes.len();
        self.boxes.push(BoxArea::default());

        //let parent = &mut self.boxes[parent_index];
        let box_area = self.boxes.last_mut().unwrap();
        let inner = box_area.inner_borrow_mut();
        // TODO: Optimize
        inner.pref_size[0] = self.pref_width.last().copied().unwrap_or_default(); 
        inner.pref_size[1] = self.pref_height.last().copied().unwrap_or_default(); 
        inner.calc_rel_position[0] = self.fixed_x.last().copied().unwrap_or_default();
        inner.calc_rel_position[1] = self.fixed_y.last().copied().unwrap_or_default();
        inner.flags = self.flags.last().copied().unwrap_or_default();
        inner.child_layout_axis = self.child_layout_axis.last().copied().unwrap_or_default();
        box_area.parent = Some(parent_index);
        index
    }
}


pub fn test() {
    /*
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
    */

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

    //println!("x = {:?}", root);
}


/*
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
    */

/* 
#[derive(Debug, Default, PartialEq)]
pub enum SizeKind {
    #[default]
    Null,
    Pixels,
    TextContent,
    PercentOfParent,
    ChildrenSum,
}
*/

/* 
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

*/

/*

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

pub fn layout_root(root: &Widget, axis: usize) {
    calc_sizes_standalone(root, axis);
    calc_sizes_upwards_dependent(root, axis);
    ui_calc_sizes_downwards_dependent(root, axis);
    enforce_constrains(root, axis);
    layout_positions(root, axis);
}
*/
