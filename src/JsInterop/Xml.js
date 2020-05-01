import * as xml from 'xmlbuilder'

export function element([elementName,elementValue],xmlPropertyList ){
    var builder = xml.create()
}

export function attr(attrList){
    var newAttrList = [];
    attrList.forEach(element => {
        var {attrName,attrVal} = element
        newAttrList.push({attrName,attrVal});
    return newAttrList;
    });
}

export function element([elementName,elementValue],xmlPropertyList ){
    var builder = xml.create()
}