VARIABLE_RESERVED_NAMES_CONST <- c( 'inflow',
                                    'joint',
                                    'context')

JOINT_TYPES_FLOW <- c('pipe', 'junction')
JOINT_TYPES_JOINTS <- c( 'processor')
JOINT_TYPES_QA <- c('warning', 'error')
JOINT_TYPES_STRUCTURE <- c('structure', 'module', 'context')

JOINT_TYPES <- c('tap', JOINT_TYPES_FLOW, JOINT_TYPES_JOINTS, JOINT_TYPES_QA, JOINT_TYPES_STRUCTURE)
JOINT_TYPES_FUN <- c(JOINT_TYPES_FLOW, JOINT_TYPES_JOINTS, JOINT_TYPES_QA)


ELEMENTS <- c("attributes", "variables", "parameters", "function", "arguments")
