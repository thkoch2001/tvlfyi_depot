use super::*;

use mmtk::util::opaque_pointer::VMWorkerThread;

pub struct TvixReferenceGlue;

impl ReferenceGlue<TvixEval> for TvixReferenceGlue {
    type FinalizableType = ObjectReference;

    fn set_referent(_reference: ObjectReference, _referent: ObjectReference) {
        unimplemented!()
    }
    fn get_referent(_object: ObjectReference) -> ObjectReference {
        unimplemented!()
    }
    fn enqueue_references(_references: &[ObjectReference], _tls: VMWorkerThread) {
        unimplemented!()
    }
}
