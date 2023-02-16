package pythonUtil;

using StringTools;

@:include("PythonHandler.cpp")
@:buildXml('<include name="../../../../source/pythonUtil/PythonBuild.xml" />')
@:unreflective
@:keep
@:native("PythonHandler*")
extern cl--- Python
{
	@:native("doFile")
	public static function doFile(str:String):Void;
}
