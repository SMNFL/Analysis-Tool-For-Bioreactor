
namespace Library

    type tableRow =
        {
            PhaseID : int;
            startTimeGrowphase : float;
            endTimeGrowphase : float;
            slopeOrGrowrateOfLinearRegressionOrGrowphase : float;
            duplicationTimeOfGrowphase : float
        }

    type Result<'T, 'TError> =
        | Ok of 'T
        | Error of 'TError
