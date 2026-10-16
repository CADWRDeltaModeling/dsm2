# Getting Started

Welcome to DSM2. This section of the documentation is intended to help you:

1. Acquire and install DSM2.
2. Test that it is working.
3. Understand the layout of the distribution.

After completing these steps, you may want to explore the tutorials in the `/tutorials` folder or consult the documentation and grid map in the `/documentation` folder. A link to the documentation is available in the start menu for easier access.

---

## Getting DSM2

DSM2 is distributed by the California Department of Water Resources Delta Modeling Section. You can find the model at the [CNRA Open Data website](https://data.cnra.ca.gov/dataset/dsm2).

Currently, we distribute:

- Windows executables
- Tutorials
- Source code
- A data package with templates for common studies

---

## Installing DSM2

DSM2 has been tested on Windows 10.

- DSM2 is distributed as a `.zip` file containing the model executables and input files.
- **Do not unzip it to a location with spaces in the path.**
- Recommended locations: `D:\delta\dsm2` or `C:\delta\dsm2`.
- Ensure the drive has sufficient space (several gigabytes) for easier in-place usage.

---

## Recommended Third-Party Tools

DSM2 comes with a numerical model and scripting capabilities. To enhance usability, we recommend the following tools:

### Command Window
Follow the instructions [here](https://www.windowscentral.com/add-open-command-window-here-back-context-menu-windows-10) to add the "Open command window here" option to the Windows Explorer context menu. This requires administrative privileges and involves modifying the Windows registry. This tool is essential for efficiently running DSM2 models and Python scripts.

### Text Editor
- **Notepad++**: A text editor that works well with DSM2 input data and integrates with Windows Explorer. Syntax highlighting support is available for DSM2.

### Diff Tool
- **DiffMerge**: A free tool for comparing text files.
- **Beyond Compare**: A commercial product that is intuitive and supports Word file comparisons.

### DSS Tools
- **Vista**: A graphical tool for examining data in HEC-DSS format, included in the `/dsm2/vista/bin` directory.
- **HEC-DSSVUE**: Distributed by HEC and actively maintained. Most users prefer DSSVUE as their primary tool, with Vista used for specific tasks. An Excel add-in for DSS data is also available on the HEC website.

### HDF5 Tools
- **HDFView** and **HDF-Explorer**: Independent browsers for the HDF5 file format. These tools allow you to inspect tidefiles, one of the model's outputs. Only one of these tools is needed.

---

## Test Launching DSM2

To verify the installation, open a command prompt and type the following command from any location:

```bash
C:\>hydro -v
DSM2-Hydro 8.2.2  Git Version: 1468 Git GUI: 54a9cc3c
Usage: Hydro input-file
```

If you see a message like the one above, the installation was successful!

If instead you see:

```bash
C:\>hydro -v
'hydro' is not recognized as an internal or external command,
operable program or batch file.
```

...you have a path issue that needs to be resolved.

---

## Next Steps

Your next step should be to explore the [Tutorials](tutorials/An_Introduction_to_DSM2_Tutorials.md).

- The **Basic Tutorials (Tutorials 1-6)** cover most of the model's nuances using a simple grid. They are an excellent way to learn about the model, including subtle features that may be new or confusing.
- The **Delta Tutorial series** focuses on applied tasks in the Delta. Starting with Delta Tutorial #1 as a motivator and then tackling the basic tutorials is a quick way to get familiar with the model.

