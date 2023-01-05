/* The index of the beginning of the farm today */
const double *phi_0 = &v[-4 * node];
/* The number of pens in the section */
const double npens_s = ldata[1] - ldata[0] + 1;
/* The number of pens in the farm */
const double npens_f = ldata[3] - ldata[2] + 1;
/* The number of infected individuals in the node */
const int nI = u[1] + u[2] + u[3] + u[5] + u[6] + u[7] + u[9] + u[10] + u[11] + u[13] + u[14] + u[15] + u[17] + u[18] + u[19];
/* The total number of individuals in the node */
const int nTOT = nI + u[0] + u[4] + u[8] + u[12] + u[16];
/* cleaning indicator of the current pen */
const int clean = u[25];
/* Record the number of individuals in the node */
v_new[3] = nTOT;

/* Decay the phi and add from shedding animals */
v_new[0] = gdata[0] * v[0] + nI;

/* Further decay with cleaning. This should only happen if the number
 * of animals in the pen today is 0 and the number of animals
 * yesterday was >0  */
if(v[3] > 0 && v_new[3] == 0) {
    v_new[0] *= gdata[7];
}

/* decay cleaning for continuous flow pens, u[25] is the cleaning indicator column */
if(clean > 0){
    v_new[0] *= gdata[8];
}

/* Sum the phi in the section */
v_new[1] = 0;
for (int i = ldata[0]; i <= ldata[1]; i += 1) {
    v_new[1] += phi_0[4*i];
}

/* Sum the phi in the farm */
v_new[2] = 0;
for (int i = ldata[2]; i <= ldata[3]; i += 1) {
    v_new[2] += phi_0[4*i];
}

/* Subtract the current node phi from the section phi */
v_new[2] -= v_new[1];
/* Subtract the the current section phi from the farm phi */
v_new[1] -= v[0];
/* Decay the section phi */
v_new[1] *= gdata[0];
/* Decay the farm phi */
v_new[2] *= gdata[0];
/* Scale the section and farm phi by the number of pens */
v_new[1] /= (npens_s - 1);
v_new[2] /= (npens_f - npens_s);

return 1;
