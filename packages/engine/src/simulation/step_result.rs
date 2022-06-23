use execution::{package::simulation::output::Output, runner::RunnerError};
use simulation_structure::SimulationShortId;

use crate::simulation::agent_control::AgentControl;

pub struct SimulationStepResult {
    // TODO: UNUSED: Needs triage
    pub sim_id: SimulationShortId,
    pub output: Vec<Output>,
    // TODO: UNUSED: Needs triage
    pub errors: Vec<RunnerError>,
    // TODO: UNUSED: Needs triage
    pub warnings: Vec<RunnerError>,
    pub agent_control: AgentControl,
}
