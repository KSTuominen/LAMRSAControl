library(LAMRSAControl)

result <- create_u0(node = TRUE,
             time = TRUE,
             capacity = TRUE,
             npigs = TRUE,
             phi = TRUE,
             s_phi = TRUE,
             f_phi = TRUE)

result <- clean_trajectory(result)
as_v_matrix(result)
