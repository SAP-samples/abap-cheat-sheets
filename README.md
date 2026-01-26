[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-cheat-sheets)](https://api.reuse.software/info/github.com/SAP-samples/abap-cheat-sheets)

# ABAP Examples Using Object-Oriented Design Patterns

The `oo_patterns` branch of the [ABAP cheat sheet GitHub repository](https://github.com/SAP-samples/abap-cheat-sheets) includes a selection of design patterns you may have come across in object-oriented programming. The example classes serve as an excursion. Unlike other ABAP cheat sheet examples, they do not cover ABAP-specific topics as such. Instead, the example classes represent ABAP code explorations and experiments related to various design patterns.

Design patterns address common software design challenges, aiming to improve modularity, scalability, reusability, and more.
The examples largely draw inspiration from design patterns established by the _Gang of Four_ (GoF), applicable across different object-oriented programming languages. Apart from selected, renowned GoF patterns, the example classes also include a random selection of other techniques.

> [!IMPORTANT]
> - The focus of the examples is on basic conceptual considerations regarding the design patterns, and experimenting with ABAP syntax and concepts (such as interfaces, abstract classes, encapsulation, and more). The examples aim to illustrate basic design pattern ideas using simplified ABAP demo classes. 
> - The examples are meant for [exploration, experimentation, and demonstration](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/README.md#%EF%B8%8F-disclaimer), using non-semantic and non-real-world contexts to reduce complexity and give a rough idea. Due to their experimental nature, these examples do not represent best practices or model approaches, as various approaches and class setup strategies may apply. It is up to you to evaluate whether the patterns are suitable and beneficial for your setup. Always create your own solutions. 
> - The [ABAP Examples Using Object-Oriented Design Patterns](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/34_OO_Design_Patterns.md) cheat sheet offers the examples in this branch as copy-and-paste example classes. You can also find explanations and additional details about the examples there.

## Getting Started

> [!NOTE]
> - The code examples in this branch are designed to function independently from those in the `main` branch. Therefore, you can clone this branch without also cloning the `main` branch.
> - The following steps outline the import procedure in the SAP BTP ABAP environment.
> - If you have already imported the `main` branch of the ABAP cheat sheet repository, the repository is still linked in the *abapGit Repositories* view, and you want to have the artifacts of the `oo_patterns` branch in the same package, you can proceed with the steps in the note below. Otherwise, open the *abapGit Repositories* view in ADT, filter for the linked repository, right-click it, and choose _Unlink_.

Use the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in to install the examples by carrying out the following steps:

1. In your ABAP cloud project, create a package, for example, *ZABAP_CHEAT_SHEETS_OODP* as the target package. The package should be local.
2. Add the package to the *Favorite Packages* in the *Project Explorer* view in ADT.
3. To add the <em>abapGit Repositories</em> view to the <em>ABAP</em> perspective, choose *Window* → *Show View* → *Other...* from the menu bar and choose *abapGit Repositories*.
4. In the <em>abapGit Repositories</em> view, choose the `+` icon in the upper right corner of the ADT tab to link a new abapGit repository.
5. The *Link abapGit Repository* popup appears. Enter the following URL. Note that if you have already imported the `main` branch of the ABAP cheat sheet repository and linked it, unlink it first as described in the note above. 

    ```
    https://github.com/SAP-samples/abap-cheat-sheets.git
    ```

6. Choose *Next*. 
7. Provide your Git user and password/token.
8. Choose *Next*. A pop-up is displayed prompting you to choose to store or delete credentials in/from secure storage.
9.  On the *Branch and Package Selection* screen, select the `oo_patterns` branch and enter the name of the created package (for example, *ZABAP_CHEAT_SHEETS_OODP*) in the *Package* field. 
10. Choose *Next*.
11. On the *Select Transport Request* screen, choose *Finish* to link the Git repository to your ABAP cloud project. The package should be local.
12. In the *abapGit Repositories* view, filter for your package. The repository appears in the *abapGit Repositories* view with the status <em>Linked</em>.
13. Right-click on the new abapGit repository and choose *Pull...* to start the cloning of the repository contents.
14. On the *Branch and Package Selection* screen, choose *Next*.
15. On the next screen, select the objects (for example, the package to automatically select all artifacts) from the list and choose *Next*.
16. On the next screen - the package should be local - choose *Finish*. Same as above, if an *object already locked* message is displayed, choose *Finish* as well. The status in the *abapGit Repositories* view changes to <em>Pull running...</em>. Note that the pull run may take several minutes.
17. Once the cloning is complete, the status changes to *Pulled Successfully*. You may need to refresh the *abapGit Repositories* view to see the progress of the import. To do this, choose the  *Refresh* icon in the upper right corner of the view.
18. Refresh your project tree. For example, in ADT, right-click the package and choose *Refresh*. The package should contain all the artifacts from the GitHub repository.
19. Make sure that all artifacts are active. To activate all inactive development objects, choose the *Activate all inactive ABAP development objects* button from the menu (or choose *CTRL+Shift+F3*).


> [!NOTE]
> If you have already imported the `main` branch of the ABAP cheat sheet repository, and linking the repository anew is rejected, proceed as follows to include the artifacts in the package of the already imported repository: 
> - Open the *abapGit Repositories* view in ADT. 
> - Right-click the line with the above GitHub URL. Choose _Switch Branch_. Provide user credentials when prompted.
> - On the *Branch Selection* screen, select the `oo_patterns` branch. The package entry remains unchanged.
> - Choose *Next*.
> - In the *abapGit Repositories* view, the _Status_ should be _Linked_.
> - Right-click the line and choose _Pull_.
> - In the _Object Selection for Pull_ screen, you may choose _Next_ without selecting any artifacts for deletion.
> - Once having finished the wizard, the _Status_ should be _Pull starting_. You can choose the *Refresh* icon to check on the progress.
> - Make sure that all artifacts are active. To activate all inactive development objects, choose the *Activate all inactive ABAP development objects* button from the menu (or choose *CTRL+Shift+F3*).



## Example Setup and Running Example Classes

Most examples are set up for easy exploration using simple, (mostly) self-contained ABAP classes, i.e. there is only one class pool including local classes/interfaces instead of multiple global classes/interfaces:
- Global class (_Global Class_ tab in ADT):
  - Includes the `if_oo_adt_classrun` interface to run the class with F9 in ADT.
  - Serves as the client that makes use of the local classes to demonstrate design patterns. Largely, the declarations and implementations in the CCIMP include are relevant for the conceptual considerations.
- CCIMP include (_Local Types_ tab in ADT):
  - Contains various local classes/interfaces to demonstrate design patterns, allowing quick copying and pasting without creating multiple global classes.

 To try the examples out, choose *F9* in ADT to execute the classes. The examples are set up to display output in the console.

> [!NOTE]
> - In the individual sections of the [ABAP Examples Using Object-Oriented Design Patterns](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/34_OO_Design_Patterns.md) cheat sheet, you can find explanations and additional details about the examples in expandable sections.
> - [Disclaimer](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/README.md#%EF%B8%8F-disclaimer)

## ⚠️ Disclaimer
The code examples presented in this repository are only syntax examples and are not intended for direct use in a production system environment. The code examples are primarily intended to provide a better explanation and visualization of the syntax and semantics of ABAP statements and not to solve concrete programming tasks. For production application programs, a dedicated solution should therefore always be worked out for each individual case.
There is no guarantee for either the correctness or the completeness of the code. In addition, there is no legal responsibility or liability for possible errors or their consequences, which occur through the use of the example code.

<br>

## 📟 Support and Contribution
This is not intended to be a contribution repository, so please do not create pull requests. If you like to address issues or suggestions, please create an issue. However, this project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

<br>

## 📜 License
Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSE) file.