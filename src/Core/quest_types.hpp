
/* *****************************************************************************
 * This file is part of the Quest Text Editor project.
 * Copyright (c) 2021 Daniel Swanson (@mr-martian), Dusty Miller (@d-us-vb),
 *   killerbee (@killerbee13), Richard (@CodeTriangle)
 *
 * This file contains all types that are used globally in the Quest System
 *
 * ****************************************************************************/


#include <vector>
#include <thread>
#include <variant>

#include "rope.hpp"

namespace Quest
{

    /*
     * Quest Core uses the Entity-Component-System (ECS) wherever possible to ensure that
     * all data in the system is easily accessible very quickly. ECS is tricky to grok at
     * first, but the intuition should generally be that all the data in the editor is
     * more or less kept in a collection of massive tables. To perform some query or
     * process data, you simply specify the columns (components), then the API provides
     * iterators for your processing function (system).
          */
    

    /* Buffers
     *
     * Many text editors have the concept of a buffer. Essentially, a cache of data for
     * files that are currently open. In Emacs world, most buffers are not visible to the
     * user nor even immediately obvious that they're available. It is not uncommon for
     * there to be several hundred buffers open simultaneously. What is missing, however,
     * is a sane method of differentiating between the different aspects of how humans
     * want to interact with data. There isn't full agreement on the Quest Team about how
     * much a text editor should be able to do. But what we all agree on is that the
     * system should be general enough to edit, manipulate, and otherwise operate on *any*
     * kind of text.
     *
     * To achieve this, Dusty proposed a 3 stage system of many-to-many relationships:
     *
     * - Raw Buffers, which contain literal file data.
     *
     * - Semantic Buffers, which contain some (ostensibly) useful interpretation of the
     *   data.
     *
     * - Display Buffers, which contain a standardized format easy for frontends to
     *   transform into widgets.
     *
     * The many-to-many system means that extensions can freely intermingle the data
     * stored in the previous stage. One semantic buffer can reference multiple raw
     * buffers and compose their data. Multiple semantic buffers can reference the same
     * raw buffer if it's useful to do so. A single display buffer (or rather, it's
     * display policy) might compose multiple semantic buffers into a single unified view,
     * and as you'd expect, multiple display buffers can simultaneously transform data
     * from the same semantic buffer.
     *
     * (There is a fourth type, the Frame Buffer, which is tightly coupled to the
     * frontend. Most users don't need to worry about frame buffers. The three wholly
     * Quest buffer types are completely decoupled from any outside system. See
     * quest_frontend_interface.hpp)
     * 
     */

    /*
     * Raw Buffers: A raw buffer contains the raw data read from a file or a
     * byte-serialized form of the data structures in a semantic buffer.
     * 
     * - RawTextBuffer: this stores plaintext read from a file. It is stored as
     *   a NonQuest::Rope.
     
     * - RawBinaryBuffer: this stores a blob of binary data.
     
     * - RawLargeTextBuffer: this stores the handle and data of a very large text file.
     *   The threshold between regular and large file is set optionally by the user.
     *   Default is 16MB. 
     
     * - RawLargeBinaryBuffer: this stores a blob of *lots* of binary data.

     * These types are combined into Quest::RawBuffer, a std::variant to help simplify the
     * code.
     *
     * In general, raw buffers are updated depending on an update policy, not necessarily
     * every key press. The rope data structure in a raw text buffer gives free tree style
     * undo/redo functionality. The update policy makes it possible to undo a chunk at a
     * time in a somewhat more semantically pleasing manner than just saying "undo all the
     * data entered during the last typing burst up to 1 second" or "the last five
     * characters". One possible raw buffer update policy is "udpate when AST gets a new
     * element added."
     *
     * But of course, should a user desire it, update every keypress is 100% a valid
     * update policy.
     */

    struct RawTextBuffer
    {
    };

    struct RawBinaryBuffer
    {
    };
    
    struct RawLargeTextBuffer
    {
    };

    struct RawLargeBinaryBuffer
    {
    };

    typedef std::variant< RawTextBuffer, RawBinaryBuffer, RawLargeTextBuffer > RawBuffer;  

    /* Semantic Buffers
     *
     * Semantic buffers store one or more structured interpretations of the file. Quest
     * natively supports 6 data structures that should cover 99.9% of files. Note that
     * *array of bytes* is not included in here, because that is supported at the
     * RawBuffer level.
     
     * 1. Arrays of lines 
     * 2. Concrete Syntax Trees
     * 3. Abstract Syntax Trees
     * 4. Multi-Column Tables 
     * 5. Hash Tables
     * 6. Hierarchical Key-Value Trees

     * Because these datastructures are largely intended to represent text, they can be
     * composed of each other in whichever way makes the most sense. For markup documents
     * (probably interfaced with an AST), it may be quite handy to include a multi-column
     * table as an element of it. If that table contains snippets of source code, it may
     * be handy to actually include CSTs or ASTs inside the cells of the table.
     */


    // these are more like mental notes. Even graphlike structures exhibit faster access
    // when stored in an ECS style component database. (Chasing pointers is expensive!)
    // Also, these structures are mostly static objects that can be queried. Navigation
    // shortcuts, display policies, and data extractors do this quite a lot. Most
    // importantly, they define an interface. When someone wants to create an extension
    // for Quest to integrate their favorite programming language or app, then they need
    // only implement the interface for the structures relevant to their language or
    // system.
    
    struct LineArray {};
    struct ConcreteSyntaxTree {};
    struct AbstractSyntaxTree {};
    struct MultiColumnTable {};
    struct HashTable {};
    struct HierarchicalKVTree {};
    
    
    struct SemanticBuffer
    {
    };

    /* Display Buffers
     *
     * The Display Buffer is the last step in Quest-land before a file can be shown to the
     * user. Display buffers expose a standard datastructure that describes how
     * the data should be displayed in the GUI framework.
     *
     * A display buffer is filled by a display policy, which is a system (in the ECS
     * sense) or set of systems that transform the data structures in one or more semantic
     * buffers into a deterministically laid out arrangement of richly formatted text and
     * other widgets if the frontend supports them (mainly images for reading markdown and
     * org documents with embedded images, but also it makes it possible to embed buttons
     * and checkboxes for configuration.)
     *
     * The display buffer does *not* handle signals or events. All it does as far as user
     * interaction is concerned is tell the frontend where to draw stuff and provide
     * identifiers for elements which get passed to Core when the frontend toolkit
     * receives a system event.

     * For example, to implement syntax highlighting from scratch, a user would write a
     * system that looks at each element in the AST and determines the correct color for
     * the element type. They would then add a tag to that region using the text styling
     * API that would automatically update the frontend-exposed data structure.
     *
     * Text substitutions are also possible in the display system stage, like replacing a
     * word formatted with markdown syntax with it's correct typeface and removing from
     * view the formatting characters. Another could be in a CSV file, replacing commas or
     * tabs with vertical bars and automatically aligning them. Yet another is simply
     * adding phantom commas between digits in large numbers.
     *
     * Display buffers can be introspected themselves by extensions. One example of this
     * is that users have the option to copy text as displayed with formatting or simply
     * the literal characters after text substitution, decoration, or annotation, as well
     * as of course copying the actual underlying text that triggered the text
     * substitution.
     *
     * Note that when text is entered on the screen, it doesn't go to the display buffer
     * directly, but instead the events are sent to Core for processing along with the
     * context signature for that location. Core executes functions associated with that
     * context signature and updates the correct semantic buffer as well, which in turn
     * triggers an update of the display buffer to show the character. This ensures that
     * the character is properly formatted before being displayed on screen.
     */
    struct DisplayBuffer
    {
    };
        
}
