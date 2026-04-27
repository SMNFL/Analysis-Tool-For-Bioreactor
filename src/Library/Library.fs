// ------------------------------------------------------------------------------------------------
//  Type definitions used throughout the analysis. 
// ------------------------------------------------------------------------------------------------

namespace Library

module Library = 
    /// Represents a row in the result table.  Each row corresponds to one growth phase.
    type tableRow =
        {
            PhaseID : int
            startTimeGrowthphase : float
            endTimeGrowthphase : float
            slopeOrGrowthrateOfLinearRegressionOrGrowthphase : float
            duplicationTimeOfGrowthphase : float
        }