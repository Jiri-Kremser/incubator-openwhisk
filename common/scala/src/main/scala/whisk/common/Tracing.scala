/*
 * Copyright 2015-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package whisk.common

import scala.collection.JavaConversions._

import io.opentracing.contrib.tracerresolver.TracerResolver
import io.opentracing.tag.Tags
import io.opentracing.util.GlobalTracer
import io.opentracing.Span
import io.opentracing.propagation.Format
import io.opentracing.propagation.TextMapExtractAdapter
import io.opentracing.propagation.TextMapInjectAdapter

object Tracing {

  case class SpanMetadata(action: String, path: String, user: String, revision: String, version: String) {
    override def toString = {
      s"SPAN[action=$action, path=$path, user=$user, revision=$revision, version=$version]"
    }
  }

  /**
    * Starts the tracing.
    */
  def startSpan(spanMetadata: SpanMetadata,
                parentOption: Option[Map[String, String]] = None,
                carrierOption: Option[scala.collection.mutable.HashMap[String, String]] = None)(implicit logging: Logging): Option[Span] = {

    // get and register the tracer if the global tracer hasn't been registered
    val existingTracerOption = Option(!GlobalTracer.isRegistered)
      .filter(identity)
      .flatMap(_ => Option(TracerResolver.resolveTracer))
    existingTracerOption foreach GlobalTracer.register
    // if the previous option is empty, get the global tracer as fallback method
    val tracerOption = Option(existingTracerOption.getOrElse(GlobalTracer.get))

    logging.warn(this, s" span               -----------------------:            $spanMetadata")
    logging.warn(this, s" parentOption       -----------------------:            ${parentOption}")
    logging.warn(this, s" carrierOption      -----------------------:            ${carrierOption}")



    tracerOption.map(tracer => {
      val spanBuilder = tracer.buildSpan(spanMetadata.action)

      // add reference to parrent if there is any and start the span
      val span = parentOption.map(parent => {
        val parentSpan = tracer.extract(Format.Builtin.TEXT_MAP, new TextMapExtractAdapter(parent))
        spanBuilder.asChildOf(parentSpan)
      }).getOrElse(spanBuilder).start
      //todo: new api has startManual

      // inject the context in case it's the high-lvl action
      carrierOption.map(carrier => {
        logging.warn(this, s" trying to inject parent      -----------------------:            ${carrier}")
        tracer.inject(span.context, Format.Builtin.TEXT_MAP, new TextMapInjectAdapter(carrier))
        logging.warn(this, s" parent injected      -----------------------:            ")
      })

      Tags.COMPONENT.set(span, "openwhisk")

      // general message data
      span.setTag("user", spanMetadata.user)
      span.setTag("revision", spanMetadata.revision)

      // action data
      span.setTag("action", spanMetadata.action)
      span.setTag("version", spanMetadata.version)
      span.setTag("path", spanMetadata.path)
      span
    });
  }

  // todo: inject pri startu, kdyz je to sekvence

  def endSpan(spanOption: Option[Span]): Unit = {
    spanOption.foreach(_.close)
  }

}
