mod free_variables;
mod var_folding;

use free_variables::{FreeVariables, Data as FVData};
use var_folding::{VarFolding, Data as VFData};
use crate::is_function_type::{IsFunctionType, Data as IFTData};
use crate::name_resolution::NameResolved as Lang;
use egg::{Analysis as EggAnalysis, EGraph};


#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Analysis(FreeVariables, IsFunctionType, VarFolding);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Data {
    pub free_variables: FVData,
    pub is_function_type: IFTData,
    pub var: VFData,
}

// Here next are some AsRef and AsMut so this analysis can be understood by the
// modules it uses such as free_variables.

impl AsRef<FVData> for Data {
    fn as_ref(&self) -> &FVData {
        &self.free_variables
    }
}

impl AsMut<FVData> for Data {
    fn as_mut(&mut self) -> &mut FVData {
        &mut self.free_variables
    }
}

impl AsRef<IFTData> for Data {
    fn as_ref(&self) -> &IFTData {
        &self.is_function_type
    }
}

impl AsMut<IFTData> for Data {
    fn as_mut(&mut self) -> &mut IFTData {
        &mut self.is_function_type
    }
}

impl AsRef<VFData> for Data {
    fn as_ref(&self) -> &VFData {
        &self.var
    }
}

impl AsMut<VFData> for Data {
    fn as_mut(&mut self) -> &mut VFData {
        &mut self.var
    }
}

impl EggAnalysis<Lang> for Analysis {
    type Data = Data;

    fn make(egraph: &EGraph<Lang, Self>, enode: &Lang) -> Self::Data {
        let free_variables = FreeVariables::make(egraph, enode);
        let is_function_type = IsFunctionType::make(egraph, enode);
        let var = VarFolding::make(egraph, enode);
        Data {
            free_variables,
            is_function_type,
            var,
        }
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> egg::DidMerge {
        self.0.merge(&mut a.free_variables, b.free_variables)
        | self.1.merge(&mut a.is_function_type, b.is_function_type)
        | self.2.merge(&mut a.var, b.var)
    }
}

impl Data {
    pub fn is_bad(&self) -> bool {
        self.var.0.as_ref().is_some_and(|x| x.is_err())
    }
}
