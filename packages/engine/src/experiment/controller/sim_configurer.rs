use std::sync::Arc;

use super::{Error, Result};

use crate::config::Globals;
use crate::config::{
    EngineConfig, ExperimentConfig, PersistenceConfig, StoreConfig, WorkerAllocation,
};
use crate::proto::{ExperimentPackageConfig, ExperimentRunBase, SimulationShortID};
use crate::SimRunConfig;

pub struct SimConfigurer {
    worker_allocator: WorkerAllocator,
}

impl SimConfigurer {
    pub fn new(package_config: &ExperimentPackageConfig, num_workers: usize) -> SimConfigurer {
        let num_workers_per_sim = match package_config {
            ExperimentPackageConfig::Simple(config) => {
                let num_runs = config.changed_properties.len();
                std::cmp::max(1, (num_workers as f64 / num_runs).ceil() as usize)
            }
            ExperimentPackageConfig::SingleRun(config) => std::cmp::max(1, num_workers),
        };

        SimConfigurer {
            worker_allocator: WorkerAllocator {
                num_workers,
                num_workers_per_sim,
                next_worker: 0,
            },
        }
    }

    pub fn configure_next(
        &mut self,
        exp_config: &Arc<ExperimentConfig<ExperimentRunBase>>,
        id: SimulationShortID,
        globals: Globals,
        store_config: StoreConfig,
        persistence_config: PersistenceConfig,
        max_num_steps: usize,
    ) -> Result<SimRunConfig<ExperimentRunBase>> {
        let worker_allocation = self.worker_allocator.next();
        let num_workers = worker_allocation.len();
        let engine = EngineConfig {
            worker_allocation,
            num_workers,
        };
        let config = SimRunConfig::new(
            &exp_config,
            id,
            globals,
            engine,
            store_config,
            persistence_config,
            max_num_steps,
        )?;
        Ok(config)
    }
}

struct WorkerAllocator {
    num_workers: usize,
    num_workers_per_sim: usize,
    next_worker: usize,
}

impl WorkerAllocator {
    pub fn next(&mut self) -> WorkerAllocation {
        (0..self.num_workers_per_sim)
            .map(|_| {
                let w = self.next_worker;
                self.next_worker = (w + 1) % self.num_workers;
                crate::config::Worker::new(w)
            })
            .collect()
    }
}
