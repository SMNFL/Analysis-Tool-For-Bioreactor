// ------------------------------------------------------------------------------------------------
//  Type definitions used throughout the analysis. 
// ------------------------------------------------------------------------------------------------

namespace Library

module Library = 
    /// Represents a row in the result table.  Each row corresponds to one growth phase.
    type tableRow =
        {
            PhaseID : int
            startTimeGrowphase : float
            endTimeGrowphase : float
            slopeOrGrowrateOfLinearRegressionOrGrowphase : float
            duplicationTimeOfGrowphase : float
        }