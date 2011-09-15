tinyMCE.init({
        // General options
        mode : "textareas",
        theme : "advanced",
	skin : "o2k7",
	language : "it",
        plugins : "table,preview,searchreplace", // ,inlinepopups

        // Theme options
        theme_advanced_buttons1 : "styleselect,formatselect,bold,italic,underline,strikethrough,|,justifyleft,justifycenter,justifyright,justifyfull,|,table,removeformat,code,|,search,replace,|,bullist,numlist,|,outdent,indent,|,undo,redo,|,link,unlink,image,code,|,preview",
        theme_advanced_buttons2 : "",
        theme_advanced_buttons3 : "",
        theme_advanced_buttons4 : "",
        theme_advanced_toolbar_location : "top",
        theme_advanced_toolbar_align : "left",
        theme_advanced_statusbar_location : "bottom",
        theme_advanced_resizing : true,

        // Example content CSS (should be your site CSS)
        content_css : "/css/reset.css,/css/screen.css",

        // Style formats
        style_formats : [
//                 {title : 'Bold text', inline : 'b'},
//                 {title : 'Red text', inline : 'span', styles : {color : '#ff0000'}},
//                 {title : 'Red header', block : 'h1', styles : {color : '#ff0000'}},
//                 {title : 'Example 1', inline : 'span', classes : 'example1'},
//                 {title : 'Example 2', inline : 'span', classes : 'example2'},
                {title : 'Tabelle:'},
	        {title : 'Bordo tabella', selector : 'table', styles : {border : '1px solid black'}},
                {title : 'Bordo cella', selector : 'td', styles : {border : '1px solid black'}},
                {title : 'Bordo titolo', selector : 'caption', styles : {border : '1px solid black'}}
        ],

        formats : {
                alignleft : {selector : 'p,h1,h2,h3,h4,h5,h6,td,th,div,ul,ol,li,table,img', classes : 'left'},
                aligncenter : {selector : 'p,h1,h2,h3,h4,h5,h6,td,th,div,ul,ol,li,table,img', classes : 'center'},
                alignright : {selector : 'p,h1,h2,h3,h4,h5,h6,td,th,div,ul,ol,li,table,img', classes : 'right'},
                alignfull : {selector : 'p,h1,h2,h3,h4,h5,h6,td,th,div,ul,ol,li,table,img', classes : 'full'},
                bold : {inline : 'span', 'classes' : 'bold'},
                italic : {inline : 'span', 'classes' : 'italic'},
                underline : {inline : 'span', 'classes' : 'underline', exact : true},
                strikethrough : {inline : 'del'},
                customformat : {inline : 'span', styles : {color : '#00ff00', fontSize : '20px'}, attributes : {title : 'My custom format'}}
        }
});
