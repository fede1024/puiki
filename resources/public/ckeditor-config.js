CKEDITOR.editorConfig = function( config )
{
	config.language = 'it';
	config.uiColor = '#e3e3e3';
	//config.baseHref = '/'; // Serve?
	
	config.contentsCss = '/css/screen.css'; // Fix
	
	config.toolbar = 'Full'; 
	
	CKEDITOR.config.toolbar_Basic = [ [ 'Source', '-', 'Bold', 'Italic' ] ];
	CKEDITOR.config.toolbar_Full = [
	['Format', 'Bold','Italic','Underline','Strike','-','Subscript','Superscript'],	['NumberedList','BulletedList','-','Outdent','Indent','-','JustifyLeft','JustifyCenter','JustifyRight','JustifyBlock'],
	['Link','Unlink'],
	['Image','Table','SpecialChar'],
	['Undo','Redo','-','Find','Replace','-','SelectAll','RemoveFormat'],
	//['Source','-','Save','Preview'],
	]; 
	CKEDITOR.config.height = '300px' ;
	
	config.extraPlugins = 'syntaxhighlight';
	config.toolbar_Full.push(['Code']);
};
