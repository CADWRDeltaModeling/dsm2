# An Introduction to DSM2 Tutorials

## DSM2 Website

[Official Website](https://water.ca.gov/Library/Modeling-and-Analysis/Bay-Delta-Region-models-and-tools/Delta-Simulation-Model-II)

## Documentation

If DSM2 is installed on your computer, click on the **START** menu and select:  
*Programs* → *DSM2_v8* → *DSM2_documentation*

---

## Introduction

Welcome to the Delta Simulation Model 2 (*DSM2*) *Version 8 tutorial*.

The tutorial is divided into two sets of lessons:  
1. **Basic DSM2 Skills**: Using simplified channels.  
2. **Advanced DSM2 Skills**: Using the model application to the Sacramento-San Joaquin Delta.  

The input files for these tutorials are located in the following directories:  
- **Basic Tutorials**: `tutorial\simple`  
- **Delta Tutorials**: `tutorial\historical`  

### Basic Tutorials

The goal of the **Basic Tutorials** (Tutorials 1-6, see Figure 1) is to familiarize you with the DSM2 input system and fundamental modeling capabilities. This six-part tutorial builds a model of a simple channel system, with each part building in complexity from its predecessor.  

> **Recommendation:** Complete the tutorials in order for the best learning experience. However, each tutorial is self-contained and can be completed independently.

![Figure 1: DSM2 Basic Tutorials](../../attachments/87228733/87228736.png)

### Delta Tutorials

The goal of the **Delta Tutorials** (Tutorials 1-5, see Figure 2) is to familiarize you with Delta-specific DSM2 applications and tasks.  

In addition, a DSM2 Overview document is provided, which describes the DSM2 modules (**HYDRO**, **QUAL**, and **PTM**) and their typical modes of application (historical, real-time, and planning).  

![Figure 2: DSM2 Delta Tutorials](../../attachments/87228733/87228735.png)

---

## DSM2 Home Directory

In working through the tutorials, the directory where you installed DSM2 will be referred to as **{DSM2_home}**.  

For example, if you accepted the default installation directory, **{DSM2_home}** would be:  
`d:\delta\dsm2` (there may also be a version number in the directory name).

---

## Tutorials Overview

### Tutorial 1: Channels

Set up the channel grid, add parameters, set boundary conditions, and list output locations.

### Tutorial 2: Reservoir Gate Transfer

Add reservoirs, gates, and transfers to the simple channel system.

### Tutorial 3: Layering

Learn how to organize data in multiple files using DSM2's data management system. Layers allow input items to be grouped logically and enable changes to be incorporated into old simulations without altering archived items.

### Tutorial 4: Time-Varying Data (Timevar)

Add time-varying information to the model. This section demonstrates how to use DSS files for boundary conditions and gate timings, replacing constant values used in earlier sections.

### Tutorial 5: Advanced Output Options

Covers advanced output options, including:  
1. Modifications to the text input file (`hydro.inp`).  
2. Use of *groups* and source tracking in QUAL.

### Tutorial 6: Operating Rules (Oprule)

Learn how to use Operating Rule Language (ORL) statements to set gate operations. ORL allows gates to operate dynamically based on conditions (e.g., automatically closing a gate when salinity reaches a threshold).

---

## Icons Used in Tutorials

- ![Gotcha Icon](../../attachments/87228733/87228734.png)  
  Indicates a DSM2 "gotcha" moment where extra care is necessary.  

- ![Question Icon](../../attachments/87228733/87228732.png)  
  Indicates a question to test your new DSM2 knowledge.

