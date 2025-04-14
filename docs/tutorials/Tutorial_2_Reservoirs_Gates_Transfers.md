# Tutorial 2: Reservoirs, Gates, Transfers

**Task**  
Add reservoirs, gates, and object-to-object flow transfers to the simple channel grid created in Tutorial 1.  

**Skills Gained**

- Understanding how reservoirs and gates are represented in DSM2.
- Learning how to transfer flow from one reservoir or node to another reservoir or node in DSM2.

The purpose of this tutorial is to learn how to add reservoirs, gates, and flow transfers to the simple channel-only grid created in Tutorial 1 (Figure 1). The grid we are going to create has the following configuration and specifications. The channel portion is identical to the simple channel model from Tutorial 1. Note that each tutorial is self-contained, so it is not necessary to complete Tutorial 1 before starting this tutorial.  

![Simple channel with a new reservoir, gate, and flow transfer](../../attachments/87228759/87228764.png)  
**Figure 1 - Simple channel with a new reservoir, gate, and flow transfer.**  

The following steps will instruct you on how to create these new features and add them to the simple channel system.  

---

### DSM2 Definitions  

#### Reservoir  
In DSM2, reservoirs are open bodies of water that store flow and are connected to nodes by means of an energy-based equation. This means that flow moves between the reservoir and its connected node or channel whenever there is an energy imbalance (e.g., stage difference). Reservoirs are considered instantly well-mixed.  

The `RESERVOIR` table specifies the identity and physical properties of the reservoir. Connections to nodes are specified in the `RESERVOIR_CONNECTION` table. If it is desired to regulate flow between a reservoir and its connected node or channel, a gate device is used.  

In DSM2 applications for the Delta, reservoirs are used for actual reservoirs such as Clifton Court Forebay and for open water bodies such as flooded islands.  

#### Gate  
In DSM2, gates are sites that present a barrier or control on flow. A gate may have an arbitrary number of associated hydraulic devices (pipes and weirs), each of which may be operated independently to control flow.  

In DSM2 applications for the Delta, gates are used to represent the Delta Cross Channel, the Montezuma Slough Salinity Control Gates, and permanent or temporary barriers.  

#### Object-to-Object Flow Transfer  
Transfers are direct water connections from a reservoir or node to another reservoir or node. Transfers are instantaneous movements of water (and its constituents and particles) without any detailed description of physics or storage. The `TRANSFER` table specifies the connectivity of the transfer.  

In DSM2 applications for the Delta, object-to-object transfers have been used to represent proposed peripheral canal withdrawal and outflow locations.  

---

### Steps to Add Reservoirs, Gates, and Transfers  

#### 1. Create the Reservoir  
1. Navigate to the directory: `${DSM2_home}\tutorial\simple\t2_reservoir_gate_transfer`.
2. Open `hydro.inp`. At the bottom of the file, add the skeleton for the `RESERVOIR` table:

```
RESERVOIR
NAME AREA BOT_ELEV
END
```

3. Enter the following values:
   - Name: `res_1`
   - Area (million sq ft): `40`
   - Bottom elevation (ft): `-24`

4. Add the `RESERVOIR_CONNECTION` table for the reservoir connections:

```
RESERVOIR_CONNECTION
RES_NAME NODE COEF_IN COEF_OUT
res_1 3 200 200
res_1 4 200 200
END
```

5. Save the file.

---

#### 2. Add Initial Conditions for the Reservoir  
1. Create the `RESERVOIR_IC` table to set the initial stage:

```
RESERVOIR_IC
RES_NAME STAGE
res_1 0.0
END
```

2. Save the file.

---

#### 3. Create the Gate  
1. Add the skeleton for the `GATE` table:

```
GATE
NAME FROM_OBJ FROM_IDENTIFIER TO_NODE
END
```

2. Enter the following values:
   - Name: `gate_1`
   - From object: `channel`
   - From identifier: `2` (refers to Channel 2)
   - To node: `2` (refers to Node 2)

3. Add the `GATE_WEIR_DEVICE` table for the weir:

```
GATE_WEIR_DEVICE
GATE_NAME DEVICE NDUPLICATE WIDTH ELEV HEIGHT CF_FROM_NODE CF_TO_NODE DEFAULT_OP
gate_1 weir 2 20 2 9999.0 0.8 0.8 gate_open
END
```

4. Add the `GATE_PIPE_DEVICE` table for the pipe:

```
GATE_PIPE_DEVICE
GATE_NAME DEVICE NDUPLICATE RADIUS ELEV CF_FROM_NODE CF_TO_NODE DEFAULT_OP
gate_1 pipe 2 2 2 0.8 0.8 gate_open
END
```

5. Save the file.

---

#### 4. Create the Transfer  
1. Add the skeleton for the `TRANSFER` table:

```
TRANSFER
NAME FROM_OBJ FROM_IDENTIFIER TO_OBJ TO_IDENTIFIER
END
```

2. Enter the following values:
   - Name: `transfer_1`
   - From object: `reservoir`
   - From identifier: `res_1`
   - To object: `node`
   - To identifier: `6`

3. Save the file.

---

#### 5. Add the Transfer Flow Time Series  
1. Add the `INPUT_TRANSFER_FLOW` table to assign a flow to the transfer:

```
INPUT_TRANSFER_FLOW
TRANSFER_NAME FILLIN FILE PATH
transfer_1 last constant 40
END
```

2. Save the file.

---

### Running HYDRO and QUAL  
1. Navigate to the directory: `${DSM2_home}\tutorial\simple\t2_reservoir_gate_transfer`.
2. Open a command window in the directory.
3. Run the following commands:
```
hydro hydro.inp
qual qual.inp
```
4. Open the `output.dss` file in the directory and examine the results.

---

### Brain Teasers  
1. Compare the equation for inflow from a node to a reservoir through a gate with the reservoir connection equation. What terms in the gate equation does the reservoir coefficient lump together?  
2. Clifton Court Forebay has five duplicate radial gates with a crest elevation of -10.1 ft and a width of 20 ft.  
   - If water is at 0 ft and the gates are open, what is the area exposed to flow?  
   - If the weirs are perfectly efficient (coefficient = 1.0), what would be the equivalent "lumped" reservoir coefficient?  
   - Was the DSM2v6 calibrated reservoir coefficient of 1800 physical? Why might it have been acceptable given the model assumptions?

