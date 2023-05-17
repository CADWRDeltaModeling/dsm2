# Debugging JNI code with Eclipse and Visual Studio (20xx)

This document shows how to setup an Eclipse project (e.g. PTM) with JNI
(native C/C++/Fortran code) with Visual Studio (e.g. 2015) 

1.  Use 32 bit version of Eclipse (e.g. eclipse-java-neon-2-win32) and
    setup PTM project
    1.  <img src="attachments/87228943/87228947.png"
        data-image-src="attachments/87228943/87228947.png"
        data-unresolved-comment-count="0" data-linked-resource-id="87228947"
        data-linked-resource-version="1" data-linked-resource-type="attachment"
        data-linked-resource-default-alias="image2020-11-10_13-14-39.png"
        data-base-url="http://msb-confluence"
        data-linked-resource-content-type="image/png"
        data-linked-resource-container-id="87228943"
        data-linked-resource-container-version="1" height="102" />Browse
        over to the checked out version of dsm2 and look under
        dsm2/src/ptm. 
    2.  Create a debug configuration. Make sure to point to the
        directory where PTM.dll is built in debug mode. E.g.
        d:\dev\dsm2\master\dsm2\BUILD\Debug\\ is where cmake builds the
        Debug version of the projects  
        <img src="attachments/87228943/87228946.png"
        data-image-src="attachments/87228943/87228946.png"
        data-unresolved-comment-count="0" data-linked-resource-id="87228946"
        data-linked-resource-version="1" data-linked-resource-type="attachment"
        data-linked-resource-default-alias="image2020-11-10_13-16-30.png"
        data-base-url="http://msb-confluence"
        data-linked-resource-content-type="image/png"
        data-linked-resource-container-id="87228943"
        data-linked-resource-container-version="1" height="400" />

2.  Start debug from Eclipse and make sure to pause on some line of code
    before JNI code is invoked. 

3.  Use Visual Studio code and make sure to be in Debug configuration.
    Then attach to the running Java process in 2 using the remote attach
    to process  
    <img src="attachments/87228943/87228945.png"
    data-image-src="attachments/87228943/87228945.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228945"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-11-10_13-22-32.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228943"
    data-linked-resource-container-version="1" height="250" />   
    <img src="attachments/87228943/87228944.png"
    data-image-src="attachments/87228943/87228944.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228944"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-11-10_13-24-53.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228943"
    data-linked-resource-container-version="1" height="400" />

    You will need to be able to identify the process in 2 by its PID or
    its name. 

4.  Set breakpoint in native code  
    <img src="attachments/87228943/87228942.png"
    data-image-src="attachments/87228943/87228942.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228942"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-11-10_13-26-14.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228943"
    data-linked-resource-container-version="1" height="400" />

5.  Release the paused Java code in the Eclipse debugger. When the
    native code trigger is hit it will stop at the above breakpoint.

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-11-10_13-26-14.png](attachments/87228943/87228942.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-11-10_13-24-53.png](attachments/87228943/87228944.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-11-10_13-22-32.png](attachments/87228943/87228945.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-11-10_13-16-30.png](attachments/87228943/87228946.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-11-10_13-14-39.png](attachments/87228943/87228947.png)
(image/png)  
